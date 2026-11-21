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
                           ;; the integer quotient of a division of doubles,
                           ;; computed into a pair of locals as a :dual
                           ((float-quotient-node-p n)
                            (list* n :fdiv (when alloc (list (new-local) (new-local :i64)))))
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
      (:fdiv (typed-float-divmod node :pair nil (cons (first locs) (second locs))))
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
    (:mul `(;; bit lengths that sum to 62 or less: the product fits
            (:local.get ,x) (:local.get ,x) (:i64.const 63) :i64.shr_s :i64.xor :i64.clz
            (:local.get ,y) (:local.get ,y) (:i64.const 63) :i64.shr_s :i64.xor :i64.clz :i64.add
            (:i64.const 66) :i64.ge_u
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
             ((:dual :fdiv) (dual-pair-i64-code (first locs) (second locs) fail))
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
             ((:dual :fdiv) (dual-materialize-code (first locs) (second locs)))
             (:i64 `((:local.get ,(first locs)) ,@(box-rep-code :i64 (node-ty leaf))))
             (:eqref `((:local.get ,(first locs)))))))))

(defun fused-tree-node-p (node)
  ;; (an operation on an integer held as an i64 beside its eqref local is
  ;; done in i64 too: the integer is not made)
  (and (not *no-fuse*) (fusable-op-node-p node)
       (some (lambda (n) (or (fusable-op-node-p n)
                             (and (eq (car n) :lref) (dual-local (second n)))
                             (float-quotient-node-p n)))
             (cddr node))
       (not (eq (fused-plan node nil) :no))))

(defun fused-parts (node)
  "For the fusable tree NODE: the code evaluating its leaves, a function of a
label giving the i64 code, the generic code, and the records of the leaves."
  (let ((recs (fused-plan node t)))
    (values (loop for rec in recs append (fused-leaf-eval-code rec))
            (lambda (fail) (let ((*fused-rest* recs)) (fused-i64-code node fail nil t)))
            (let ((*fused-rest* recs)) (fused-generic-code node t))
            recs)))

(defun f64-check-code (name x y r fail)
  "After an f64 operation NAME left its result in R (operands in X and Y):
branch to FAIL when the result is not finite (what the generic operation
decides; an underflow is no condition)."
  (declare (ignore x y))
  `(,@(f64-not-finite-code name r) (:br_if ,fail)))

(defun fused-f64-code (node fail &optional top)
  "For a fused tree whose operations are + - * with a double among the
operands of each: (values code double-p) leaving the f64 value of NODE from
the leaves of *FUSED-REST* (an operand that is not a double branches to
FAIL); NIL when the tree is not of that kind (*FUSED-REST* is then spent)."
  (cond ((and (fusable-op-node-p node) (or top (not (ty-int64-p (node-ty node)))))
         (multiple-value-bind (op a b) (fused-operands node)
           (unless (member op '(:add :sub :mul)) (return-from fused-f64-code nil))
           (multiple-value-bind (ca da) (fused-f64-code a fail)
             (unless ca (return-from fused-f64-code nil))
             (multiple-value-bind (cb db) (fused-f64-code b fail)
               (unless (and cb (or (eq da t) (eq db t))) (return-from fused-f64-code nil))
               (let ((x (new-local :f64)) (y (new-local :f64)) (r (new-local :f64))
                     (name (ecase op (:add '+) (:sub '-) (:mul '*))))
                 (values `(,@ca (:local.set ,x) ,@cb (:local.set ,y)
                           (:local.get ,x) (:local.get ,y) ,(ecase op (:add :f64.add) (:sub :f64.sub) (:mul :f64.mul))
                           (:local.set ,r)
                           ,@(f64-check-code name x y r fail)
                           (:local.get ,r))
                         t))))))
        (t
         ;; a leaf: T when it is a double (tested), :INT when an integer
         ;; (converted, as contagion does), NIL otherwise
         (destructuring-bind (leaf kind &rest locs) (pop *fused-rest*)
           (ecase kind
             (:const (let ((v (second leaf)))
                       (typecase v
                         (double-float (values `((:f64.const ,v)) t))
                         (single-float (values `((:f64.const ,(coerce v 'double-float))) :int))
                         ((signed-byte 53) (values `((:f64.const ,(coerce v 'double-float))) :int))
                         (t nil))))
             ((:dual :fdiv) nil)
             (:i64 (values `((:local.get ,(first locs)) :f64.convert_i64_s) :int))
             (:eqref (if (logtest (ty-bits (node-ty leaf)) +k-double+)
                         (values `((:local.get ,(first locs)) (:ref.test :double) :i32.eqz (:br_if ,fail)
                                   (:local.get ,(first locs)) (:ref.cast! :double) (:struct.get :double 0))
                                 t)
                         nil)))))))

(defun fused-code (node rep &optional target)
  "NODE (a fused tree) computed in i64 and left as REP (:eqref, or :i32 when
its type is a fixnum), the generic code when that fails.  When its operands
may be doubles (operands of unknown type, with a double among the operands
of each operation), the tree is tried in f64 too, before the generic code.
With TARGET, (B . F) the locals of an unboxed double variable: nothing is
left, the value is stored there."
  (let ((done (gensym "FUSED")) (fail (gensym "SLOW")) (fail2 (gensym "SLOWF")) (r (new-local :i64)))
    (multiple-value-bind (leaves i64-code generic recs) (fused-parts node)
      `(,@leaves
        (:block ,done ,(if target nil rep)
          (:block ,fail nil
            ,@(funcall i64-code fail)
            ,@(if (or (eq rep :i32) (ty-fixnum-p (node-ty node)))
                  `(:i32.wrap_i64 ,@(when (eq rep :eqref) '(:ref.i31)))
                  `((:local.tee ,r) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                    (:if nil :eqref ((:local.get ,r) :i32.wrap_i64 :ref.i31) ((:local.get ,r) (:call :i64-int)))))
            ,@(when target `((:local.set ,(car target))))
            (:br ,done))
          ,@(when (eq rep :eqref)
              (let ((code (let ((*fused-rest* recs)) (fused-f64-code node fail2 t))))
                (when code
                  `((:block ,fail2 nil ,@code
                      ,@(if target
                            `((:local.set ,(cdr target)) (:ref.null :eq) (:local.set ,(car target)))
                            '((:struct.new :double)))
                      (:br ,done))))))
          ,@generic
          ,@(when target `((:local.set ,(car target))))
          ,@(when (eq rep :i32) (unbox-rep-code :i32 (node-ty node))))))))

;;; Arithmetic on doubles of unknown type.  A tree of + - * / (and SQRT)
;;; whose operands the compiler cannot type (structure slots, list elements,
;;; parameters) but that may be doubles is computed speculatively in f64: the
;;; operands are evaluated in order, each one that is not known to be a
;;; double is tested, and only the root is boxed.  When an operand is not a
;;; double, or a step gives an infinity, a NaN or (SQRT) a negative operand, the tree is computed again by the
;;; generic operations: the operands already evaluated from their locals, the
;;; others evaluated there, in their order (so the generic code signals what
;;; it would have signalled, after the same evaluations).
;;;
;;; The operands: :D known doubles (and double constants), :U operands of
;;; unknown type that may be doubles (tested), :I fixnums (and fixnum
;;; constants), :S single-float constants (exact as doubles).  An operation
;;; belongs to the tree when one of its operands is a double (:D, :U or an
;;; operation of the tree): then, in the fast path, the other is converted
;;; exactly as contagion does.

(defun spec-op-node-p (node)
  (and (eq (car node) :prim)
       (let ((name (prim-name (second node))) (n (length (cddr node))))
         (or (and (member name '(+ - * /)) (= n 2))
             (and (eq name 'sqrt) (= n 1))))))

(defun spec-leaf-class (node)
  (if (eq (car node) :const)
      (let ((v (second node)))
        (typecase v
          (double-float :d)
          (single-float :s)
          ((signed-byte 30) :i)
          (t nil)))
      (let ((ty (node-ty node)))
        (cond ((and (eq (car node) :lref) (fdual-local (second node)) (eq (car (get-loc (second node))) :local))
               :f)
              ((ty-double-p ty) :d)
              ((ty-fixnum-p ty) :i)
              ((logtest (ty-bits ty) +k-double+) :u)
              (t nil)))))

(defun spec-double-bearing-p (node)
  "NODE is a double in the fast path: a :D or :U operand, or an operation
of the tree."
  (if (and (spec-op-node-p node) (not (ty-double-p (node-ty node))))
      (spec-op-p node)
      (member (spec-leaf-class node) '(:d :u :f))))

(defun spec-op-p (node)
  "NODE (an operation) belongs to a speculative tree."
  (let ((args (cddr node)))
    (if (eq (prim-name (second node)) 'sqrt)
        (spec-double-bearing-p (first args))
        (and (some #'spec-double-bearing-p args)
             (every (lambda (a) (or (spec-double-bearing-p a) (member (spec-leaf-class a) '(:i :s))))
                    args)))))

(defun spec-tree-stats (node)
  "(values number-of-operations has-an-operand-of-unknown-type
has-a-double-operand) of the tree NODE (an unboxed double variable is both)."
  (let ((ops 0) (u nil) (d nil))
    (labels ((walk (n)
               (if (and (spec-op-node-p n) (not (ty-double-p (node-ty n))) (spec-op-p n))
                   (progn (incf ops)
                          ;; (a square root of doubles may be complex: tested too)
                          (when (eq (prim-name (second n)) 'sqrt) (setq u t))
                          (mapc #'walk (cddr n)))
                   (case (spec-leaf-class n)
                     ;; (a value that is a double on one of its paths: also evidence)
                     (:u (setq u t) (when (dual-f64-node-p n) (setq d t)))
                     (:d (setq d t))
                     (:f (setq u t d t))))))
      (walk node))
    (values ops u d)))

(defun spec-tree-has-u-p (node) (nth-value 1 (spec-tree-stats node)))

(defun spec-tree-node-p (node &optional evidence)
  "NODE is computed as a speculative tree: it has two operations or more,
or (with EVIDENCE, or an unboxed double variable among its operands) one
operation and a double among its operands."
  (and (not *no-fuse*) (spec-op-node-p node) (spec-op-p node)
       (multiple-value-bind (ops u d) (spec-tree-stats node)
         (and u (if evidence d (or (>= ops 2)
                                   (some (lambda (a) (or (eq (spec-leaf-class a) :f)
                                                         (and (eq (spec-leaf-class a) :u) (dual-f64-node-p a))))
                                         (cddr node))))))))

(defun spec-tree-code (node &optional target)
  "The value (an eqref) of the speculative tree NODE.  With TARGET, (B . F)
the locals of an unboxed double variable: nothing is left, the value is
stored there (F, with B null, when the fast path gave it)."
  (let ((leaves nil) (count 0)
        (done (new-local :i32)) (bail (gensym "SPECBAIL")) (out (gensym "SPEC")))
    (labels ((op-p (n) (and (spec-op-node-p n) (not (ty-double-p (node-ty n))) (spec-op-p n)))
             (fast (n)
               (if (op-p n)
                   (let* ((name (prim-name (second n))) (args (cddr n))
                          (x (new-local :f64)) (r (new-local :f64)))
                     (if (eq name 'sqrt)
                         `(,@(fast (first args)) (:local.tee ,x)
                           ;; a negative operand (or NaN) has a complex root
                           (:f64.const 0) :f64.ge :i32.eqz
                           (:if nil nil ((:br ,bail)))
                           (:local.get ,x) :f64.sqrt)
                         (let ((y (new-local :f64)))
                           `(,@(fast (first args)) (:local.set ,x)
                             ,@(fast (second args)) (:local.set ,y)
                             (:local.get ,x) (:local.get ,y)
                             ,(ecase name (+ :f64.add) (- :f64.sub) (* :f64.mul) (/ :f64.div))
                             (:local.set ,r)
                             ;; not finite
                             ,@(f64-not-finite-code name r)
                             (:if nil nil ((:br ,bail)))
                             (:local.get ,r)))))
                   (leaf n)))
             (leaf (n)
               (let ((class (spec-leaf-class n)))
                 (when (eq class :f)
                   ;; an unboxed double variable (no side effect: the
                   ;; generic code reads it again)
                   (let ((b (cdr (get-loc (second n)))) (f (fdual-local (second n))))
                     (return-from leaf
                       `((:local.get ,b) :ref.is_null
                         (:if nil :f64
                              ((:local.get ,f))
                              ((:local.get ,b) (:ref.test :double) :i32.eqz (:br_if ,bail)
                               (:local.get ,b) (:ref.cast! :double) (:struct.get :double 0)))))))
                 (when (and (eq class :u) (dual-f64-node-p n))
                   ;; a double on one of its paths: computed into a pair of
                   ;; locals (CG-DUAL-F64), tested only when not unboxed
                   (let ((b (new-local)) (f (new-local :f64)) (i (incf count)))
                     (push (list n i :dual (cons b f)) leaves)
                     (return-from leaf
                       `(,@(cg-dual-f64 n b f) (:i32.const ,i) (:local.set ,done)
                         (:local.get ,b) :ref.is_null
                         (:if nil :f64
                              ((:local.get ,f))
                              ((:local.get ,b) (:ref.test :double) :i32.eqz (:br_if ,bail)
                               (:local.get ,b) (:ref.cast! :double) (:struct.get :double 0)))))))
                 (if (eq (car n) :const)
                     (let ((v (second n)))
                       `((:f64.const ,(coerce v 'double-float))))
                     (let* ((rep (ecase class (:d :f64) (:i :i32) (:u :eqref)))
                            (loc (new-local rep)) (i (incf count)))
                       (push (list n i rep loc) leaves)
                       (ecase class
                         (:d `(,@(cg-rep n :f64) (:local.tee ,loc) (:i32.const ,i) (:local.set ,done)))
                         (:i `(,@(cg-rep n :i32) (:local.tee ,loc) (:i32.const ,i) (:local.set ,done)
                               :f64.convert_i32_s))
                         (:f (error "unreachable"))
                         (:u `(,@(cg n) (:local.tee ,loc) (:i32.const ,i) (:local.set ,done)
                               (:ref.test :double) :i32.eqz (:if nil nil ((:br ,bail)))
                               (:local.get ,loc) (:ref.cast! :double) (:struct.get :double 0))))))))
             (generic (n)
               (if (op-p n)
                   (funcall (prim-fn (second n)) (mapcar #'generic (cddr n)))
                   (if (or (eq (car n) :const) (eq (spec-leaf-class n) :f))
                       (cg n)
                       (destructuring-bind (i rep loc) (cdr (assoc n leaves))
                         `((:local.get ,done) (:i32.const ,i) :i32.ge_u
                           (:if nil :eqref
                                ,(if (eq rep :dual)
                                     (fdual-materialize-code (car loc) (cdr loc))
                                     `((:local.get ,loc) ,@(ecase rep (:f64 '((:struct.new :double))) (:i32 '(:ref.i31))
                                                             (:eqref nil))))
                                (,@(cg n)))))))))
      (let ((fast-code (fast node)))
        (if target
            `((:i32.const 0) (:local.set ,done)
              (:block ,out nil
                (:block ,bail nil
                  ,@fast-code (:local.set ,(cdr target)) (:ref.null :eq) (:local.set ,(car target)) (:br ,out))
                ,@(generic node) (:local.set ,(car target))))
            `((:i32.const 0) (:local.set ,done)
              (:block ,out :eqref
                (:block ,bail nil
                  ,@fast-code (:struct.new :double) (:br ,out))
                ,@(generic node))))))))

(defun mulmod-node-args (p args)
  "For (MOD (* a b) c) / (REM (* a b) c): the nodes a, b, c; else NIL.  The
operations are done after C is evaluated, so C must be a variable or a
constant unless the product cannot signal."
  (when (and (member (prim-name p) '(mod rem)) (= (length args) 2))
    (let ((m (first args)) (c (second args)))
      (when (and (eq (car m) :prim) (eq (prim-name (second m)) '*) (= (length (cddr m)) 2)
                 (not (ty-int64-p (node-ty m)))
                 (or (member (car c) '(:lref :const))
                     (and (fused-int-node-p (third m)) (fused-int-node-p (fourth m)))))
        (list (third m) (fourth m) c)))))

(defun cg-prim (p args mv)
  ;; (not x) / (null x): the test of X negated, as T or NIL (no T/NIL of X
  ;; in between when X is itself a test: (not (null x)), (not (< a b c)))
  (when (and (member (prim-name p) '(not null)) (= (length args) 1))
    (return-from cg-prim `(,@(cg-test `(:prim ,p ,@args)) ,@(bool-code) ,@(when mv (mv-one)))))
  (let ((mm (mulmod-node-args p args)))
    (when mm
      (return-from cg-prim
        `(,@(cg (first mm)) ,@(cg (second mm)) ,@(cg (third mm))
          (:i32.const ,(if (eq (prim-name p) 'mod) 1 0)) (:call :mulmod) ,@(when mv (mv-one))))))
  ;; a double among the operands: f64 first
  (when (spec-tree-node-p `(:prim ,p ,@args) t)
    (return-from cg-prim
      `(,@(spec-tree-code `(:prim ,p ,@args)) ,@(when mv (mv-one)))))
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
;; (sys:%signal-type-error datum type): signal the TYPE-ERROR (out of line: the
;; code of the function that calls it needs nothing for the exits it may make)
(defprim clysm-sys::%signal-type-error (x type) :value
  `(,@x ,@type ,@(lisp-call-instrs 'clysm-sys::%type-error 2) :unreachable))
(defprim clysm-sys::%bignump (x) :test `(,@x (:ref.test :bignum)))
;; INTEGERP as a test the compiler decides from the type of X (src/typed.lisp)
;; where it can: the check of a value given to a variable of an integer type
(defprim clysm-sys::%integerp (x) :test `(,@x (:call :integerp)))
(defprim clysm-sys::%ratiop (x) :test `(,@x (:ref.test :ratio)))
(defprim clysm-sys::%single-float-p (x) :test `(,@x (:ref.test :single)))
(defprim clysm-sys::%double-float-p (x) :test `(,@x (:ref.test :double)))
(defprim clysm-sys::%complexp (x) :test `(,@x (:call :complexp)))
(defprim clysm-sys::%complex-d-p (x) :test `(,@x (:ref.test :complex-d)))
(defprim clysm-sys::%complex-s-p (x) :test `(,@x (:ref.test :complex-s)))
(defprim clysm-sys::%simple-string-p (x) :test `(,@x (:ref.test :string)))
(defprim clysm-sys::%simple-vector-p (x) :test `(,@x (:ref.test :vector)))
;; the predicates that are a type test (runtime/seq.lisp, numbers.lisp define the functions)
(defprim simple-vector-p (x) :test `(,@x (:ref.test :vector)))
(defprim simple-bit-vector-p (x) :test `(,@x (:ref.test :bits)))
(defprim clysm-sys::double-float-p (x) :test `(,@x (:ref.test :double)))
(defprim clysm-sys::single-float-p (x) :test `(,@x (:ref.test :single)))
(defprim clysm-sys::ratiop (x) :test `(,@x (:ref.test :ratio)))
(defprim clysm-sys::bignump (x) :test `(,@x (:ref.test :bignum)))
(defprim clysm-sys::%bytes-p (x) :test `(,@x (:ref.test :bytes)))
(defprim clysm-sys::%bits-p (x) :test `(,@x (:ref.test :bits)))
(defprim clysm-sys::%f64vec-p (x) :test `(,@x (:ref.test :f64vec)))
(defprim clysm-sys::%mdarray-p (x) :test `(,@x (:ref.test :mdarray)))
(defprim clysm-sys::%instancep (x) :test `(,@x (:ref.test :instance)))
;; an immediate object (a fixnum or a character)
(defprim clysm-sys::%i31p (x) :test `(,@x (:ref.test :i31)))
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

(defun inline-fixnum-pair-code (ta tb x y slow)
  "Code branching to SLOW unless the eqref locals TA and TB hold fixnums,
leaving them in the i32 locals X and Y."
  `((:local.get ,ta) (:ref.test :i31) (:local.get ,tb) (:ref.test :i31) :i32.and :i32.eqz (:hint 0) (:br_if ,slow)
    ;; (characters are i31s outside the fixnum range)
    (:local.get ,ta) (:ref.cast! :i31) :i31.get_s (:local.tee ,x) (:i32.const ,+char-tag+) :i32.add
    (:local.get ,tb) (:ref.cast! :i31) :i31.get_s (:local.tee ,y) (:i32.const ,+char-tag+) :i32.add :i32.or
    (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:hint 0) (:br_if ,slow)))

(defun inline-fixnum-op (a b op helper)
  "Inline fixnum fast path; falls back to HELPER."
  (let ((ta (new-local)) (tb (new-local)) (x (new-local :i32)) (y (new-local :i32))
        (slow (gensym "SLOW")) (done (gensym "FX")))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:block ,done :eqref
        (:block ,slow nil
          ,@(inline-fixnum-pair-code ta tb x y slow)
          (:local.get ,x) (:local.get ,y) ,op (:local.tee ,x)
          (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:hint 0) (:br_if ,slow)
          (:local.get ,x) :ref.i31 (:br ,done))
        (:local.get ,ta) (:local.get ,tb) (:call ,helper)))))

(defun inline-fixnum-mul (a b)
  "Inline fixnum product (in i64); falls back to the :mul helper."
  (let ((ta (new-local)) (tb (new-local)) (x (new-local :i32)) (y (new-local :i32)) (r (new-local :i64))
        (slow (gensym "SLOW")) (done (gensym "FX")))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:block ,done :eqref
        (:block ,slow nil
          ,@(inline-fixnum-pair-code ta tb x y slow)
          (:local.get ,x) :i64.extend_i32_s (:local.get ,y) :i64.extend_i32_s :i64.mul (:local.tee ,r)
          (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.ge_u (:hint 0) (:br_if ,slow)
          (:local.get ,r) :i32.wrap_i64 :ref.i31 (:br ,done))
        (:local.get ,ta) (:local.get ,tb) (:call :mul)))))

(defun inline-fixnum-cmp (a b op helper-code &optional swap)
  "Inline comparison of two fixnums; falls back to HELPER-CODE."
  (let ((ta (new-local)) (tb (new-local)) (x (new-local :i32)) (y (new-local :i32))
        (slow (gensym "SLOW")) (done (gensym "FX")))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:block ,done :i32
        (:block ,slow nil
          ,@(inline-fixnum-pair-code ta tb x y slow)
          (:local.get ,x) (:local.get ,y) ,op (:br ,done))
        ,@(if swap `((:local.get ,tb) (:local.get ,ta)) `((:local.get ,ta) (:local.get ,tb)))
        ,@helper-code))))

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
(defprim /= (a b) :test (inline-fixnum-cmp a b :i32.ne '((:call :num-eq) :i32.eqz)))
(defprim zerop (a) :test (inline-fixnum-cmp a (fixnum-code 0) :i32.eq '((:call :num-eq))))
(defprim plusp (a) :test (inline-fixnum-cmp (fixnum-code 0) a :i32.lt_s '((:call :lt))))
(defprim minusp (a) :test (inline-fixnum-cmp a (fixnum-code 0) :i32.lt_s '((:call :lt))))

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
(defun constant-char-payload (code)
  "The i31 payload of CODE when it pushes a constant character, else NIL."
  (and (= (length code) 2) (consp (first code)) (eq (car (first code)) :i32.const)
       (eq (second code) :ref.i31) (integerp (second (first code)))
       (>= (second (first code)) +char-tag+)
       (second (first code))))
;; a constant operand needs no check
(defun inline-char-payload (code)
  "CODE pushes a value: push its character payload, checked inline (the
helper signals the type error)."
  (let ((tmp (new-local)) (v (new-local :i32)))
    `(,@code (:local.tee ,tmp) (:ref.test :i31)
      (:if nil :i32
           ((:local.get ,tmp) (:ref.cast! :i31) :i31.get_s (:local.tee ,v) (:i32.const ,+char-tag+) :i32.ge_s
            (:if nil :i32 ((:local.get ,v)) ((:local.get ,tmp) (:call :check-char))))
           ((:local.get ,tmp) (:call :check-char))))))
(defprim char= (a b) :test
  (let ((ka (constant-char-payload a)) (kb (constant-char-payload b)))
    (cond (kb `(,@(inline-char-payload a) (:i32.const ,kb) :i32.eq))
          (ka `(,@(inline-char-payload b) (:i32.const ,ka) :i32.eq))
          (t `(,@a (:call :check-char) ,@b (:call :check-char) :i32.eq)))))

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
  (let ((tg (new-local '(:ref :null :closure))) (t1 (new-local '(:ref :null :closure))) (tc (new-local))
        (t2 (new-local '(:ref :null :closure))))
    `(,@g (:ref.cast :closure) (:local.set ,tg) ,@f1 (:ref.cast :closure) (:local.set ,t1)
      ,@f2 (:ref.cast :closure) (:local.set ,t2)
      (:local.get ,tg) ,@(clo-field-code 0)
      (:local.get ,t1) ,@(clo-field-code 1)
      (:local.get ,t2) ,@(clo-field-code 2)
      (:local.get ,tg) ,@(clo-field-code 3)
      (:local.get ,tg) ,@(clo-field-code 4)
      (:local.get ,tg) ,@(clo-field-code +clo-entry-n+)
      (:ref.null :func) (:ref.null :eq) (:ref.func :apply-generic)
      (:struct.new :code)
      (:local.set ,tc) (:local.get ,tg) (:local.get ,tc) (:call :closure-with-code) (:ref.cast :closure))))
;; %MAKE-GF-CLOSURE4 with the keyed main function and the key layout of K
(defprim clysm-sys::%make-gf-closure4k (g f1 f2 f3 f4 k) :value
  (let ((tg (new-local '(:ref :null :closure))) (t1 (new-local '(:ref :null :closure)))
        (t2 (new-local '(:ref :null :closure))) (t3 (new-local '(:ref :null :closure)))
        (t4 (new-local '(:ref :null :closure))) (tk (new-local '(:ref :null :closure))) (tc (new-local)))
    `(,@g (:ref.cast :closure) (:local.set ,tg) ,@f1 (:ref.cast :closure) (:local.set ,t1)
      ,@f2 (:ref.cast :closure) (:local.set ,t2) ,@f3 (:ref.cast :closure) (:local.set ,t3)
      ,@f4 (:ref.cast :closure) (:local.set ,t4) ,@k (:ref.cast :closure) (:local.set ,tk)
      (:local.get ,tg) ,@(clo-field-code 0)
      (:local.get ,t1) ,@(clo-field-code 1)
      (:local.get ,t2) ,@(clo-field-code 2)
      (:local.get ,t3) ,@(clo-field-code 3)
      (:local.get ,t4) ,@(clo-field-code 4)
      (:local.get ,tg) ,@(clo-field-code +clo-entry-n+)
      (:local.get ,tk) ,@(clo-field-code +clo-kmain+)
      (:local.get ,tk) ,@(clo-field-code +clo-klayout+)
      (:ref.func :apply-generic)
      (:struct.new :code)
      (:local.set ,tc) (:local.get ,tg) (:local.get ,tc) (:call :closure-with-code) (:ref.cast :closure))))
(defprim clysm-sys::%make-gf-closure4 (g f1 f2 f3 f4) :value
  ;; like %MAKE-GF-CLOSURE with the entries for one to four arguments (G
  ;; itself may be given for an entry that is not specialized)
  (let ((tg (new-local '(:ref :null :closure))) (t1 (new-local '(:ref :null :closure))) (tc (new-local))
        (t2 (new-local '(:ref :null :closure))) (t3 (new-local '(:ref :null :closure)))
        (t4 (new-local '(:ref :null :closure))))
    `(,@g (:ref.cast :closure) (:local.set ,tg) ,@f1 (:ref.cast :closure) (:local.set ,t1)
      ,@f2 (:ref.cast :closure) (:local.set ,t2) ,@f3 (:ref.cast :closure) (:local.set ,t3)
      ,@f4 (:ref.cast :closure) (:local.set ,t4)
      (:local.get ,tg) ,@(clo-field-code 0)
      (:local.get ,t1) ,@(clo-field-code 1)
      (:local.get ,t2) ,@(clo-field-code 2)
      (:local.get ,t3) ,@(clo-field-code 3)
      (:local.get ,t4) ,@(clo-field-code 4)
      (:local.get ,tg) ,@(clo-field-code +clo-entry-n+)
      (:ref.null :func) (:ref.null :eq) (:ref.func :apply-generic)
      (:struct.new :code)
      (:local.set ,tc) (:local.get ,tg) (:local.get ,tc) (:call :closure-with-code) (:ref.cast :closure))))
;; the function of a symbol is (EQ) a given function
(defun symbol-ref-code (code)
  "CODE leaving a symbol, cast to one unless it reads the global of a
symbol (which has the type)."
  (if (and (= (length code) 1) (consp (car code)) (eq (caar code) :global.get)
           (consp (second (car code))) (eq (car (second (car code))) :sym))
      code
      `(,@code (:ref.cast :symbol))))
(defprim clysm-sys::%function-cell-eq (sym f) :test
  `(,@(symbol-ref-code sym) (:struct.get :symbol ,+sym-function+) ,@f :ref.eq))
;; the function of a symbol is the one of the definition that a fasl
;; recorded with the mark MARK (a guarded copy of its body, LIVE-INLINE-CALL):
;; it is compared with a global of the module, one for each function and
;; mark, that a toplevel form after the first form using it sets
;; (%FASL-INLINE-GUARD-SET: SYS::%FASL-INLINE-FUNCTION); before, the global
;; is null and the guard fails (the call is made)
(defun fasl-guard-global (sym mark)
  (let ((key (cons sym mark)))
    (or (gethash key *fasl-guard-globals*)
        (let ((g (list :ltv (incf *ltv-counter*))))
          (push (list g :eqref t '((:ref.null :eq))) *misc-globals*)
          (setf (gethash key *fasl-guard-globals*) g)))))
(defprim clysm-sys::%fasl-inline-guard (sym mark) :test
  `(,@(symbol-ref-code sym) (:struct.get :symbol ,+sym-function+)
    (:global.get ,(fasl-guard-global sym mark)) :ref.eq))
(defprim clysm-sys::%fasl-inline-guard-set (sym mark) :value
  (let ((g (fasl-guard-global sym mark)))
    `(,@sym ,@mark ,@(lisp-call-instrs 'clysm-sys::%fasl-inline-function 2)
      (:global.set ,g) (:global.get ,g))))
(defprim clysm-sys::%closure-key-layout (f) :value
  (let ((tmp (new-local)))
    `(,@f (:ref.cast :closure) ,@(clo-field-code +clo-klayout+) (:local.tee ,tmp) :ref.is_null
      (:if nil :eqref ,(nil-code) ((:local.get ,tmp))))))
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

(defun idx-code (i)
  "The code of the index I (code of a fixnum) as an i32.  An integer the
code has just boxed, from an i32 (an index computed with + in an i32, as
(+ I OFFSET)), is used as it is: an index beyond the fixnums is out of the
bounds of the array either way."
  (let ((last (car (last i))))
    (if (or (equal last '(:call :box-i32)) (eq last :ref.i31))
        (butlast i)
        `(,@i (:ref.cast! :i31) :i31.get_s))))

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
;; a new simple vector with the elements of the simple vector V
(defprim clysm-sys::%copy-simple-vector (v) :value
  (let ((src (new-local '(:ref :null :vector))) (dst (new-local '(:ref :null :vector))))
    `(,@v (:ref.cast :vector) (:local.tee ,src) :array.len (:array.new_default :vector) (:local.set ,dst)
      (:local.get ,dst) (:i32.const 0) (:local.get ,src) (:i32.const 0) (:local.get ,src) :array.len
      (:array.copy :vector :vector)
      (:local.get ,dst))))
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
      (:local.get ,d) (:call :dims-2d-0) (:local.get ,d) (:call :dims-2d-1) (:local.get ,d) (:call :dims-1d)
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
;; the dimension of a one-dimensional array header, else 0
(defprim clysm-sys::%mdarray-dim1 (a) :value `(,@a (:ref.cast :mdarray) (:struct.get :mdarray 7) :ref.i31))
;; setting the dimensions also sets the rank-2 dimension fields
(defprim clysm-sys::%set-mdarray-dims (a v) :value
  (let ((m (new-local '(:ref :null :mdarray))) (tmp (new-local)))
    `(,@a (:ref.cast :mdarray) (:local.tee ,m) ,@v (:local.tee ,tmp) (:struct.set :mdarray 2)
      (:local.get ,m) (:local.get ,tmp) (:call :dims-2d-0) (:struct.set :mdarray 5)
      (:local.get ,m) (:local.get ,tmp) (:call :dims-2d-1) (:struct.set :mdarray 6)
      (:local.get ,m) (:local.get ,tmp) (:call :dims-1d) (:struct.set :mdarray 7)
      (:local.get ,tmp))))

(defconstant +class-inherits-slot+ 20)   ; runtime/class0.lisp +CLASS-INHERITS+
(defconstant +class-shape-ops-slot+ 21)  ; runtime/class0.lisp +CLASS-SHAPE-OPS+

(defun inherits-check-code (l c fallback)
  "Code pushing 1 when the layout (local L, not C) has C among its included
structures: the vector of included structures of each (+CLASS-INHERITS+), by
depth, answers in constant time; FALLBACK (code pushing an i32) when either
has none (a class that is not a structure class)."
  (let ((li (new-local)) (ci (new-local)) (lv (new-local '(:ref :null :vector))) (d (new-local :i32)))
    `((:local.get ,l) (:ref.test :instance) (:local.get ,c) (:ref.test :instance) :i32.and
      (:if nil :i32
           ((:local.get ,l) (:ref.cast! :instance) (:struct.get :instance 1) (:i32.const ,+class-inherits-slot+)
            (:array.get :vector) (:local.tee ,li) (:ref.test :vector)
            (:local.get ,c) (:ref.cast! :instance) (:struct.get :instance 1) (:i32.const ,+class-inherits-slot+)
            (:array.get :vector) (:local.tee ,ci) (:ref.test :vector) :i32.and
            (:if nil :i32
                 ((:local.get ,ci) (:ref.cast! :vector) :array.len (:i32.const 1) :i32.sub (:local.tee ,d)
                  (:local.get ,li) (:ref.cast! :vector) (:local.tee ,lv) :array.len :i32.lt_u
                  (:if nil :i32 ((:local.get ,lv) (:local.get ,d) (:array.get :vector) (:local.get ,c) :ref.eq)
                       ((:i32.const 0))))
                 ,fallback))
           ,fallback))))

(defun struct-check-code (o c)
  "Push 1 when local O is a structure of class (local C): its layout is C, or
C is one of the structures the layout includes (in constant time), or (out
of line, for other classes) C is in its precedence list."
  (let ((l (new-local)))
    `((:local.get ,o) (:ref.test :instance)
      (:if nil :i32
           ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 0) (:local.tee ,l) (:local.get ,c) :ref.eq
            (:if nil :i32 ((:i32.const 1))
                 ,(inherits-check-code l c `((:local.get ,o) (:local.get ,c) (:call :struct-typep)))))
           ((:i32.const 0))))))

;;; structure slot access: inline when the object is of the structure (or
;;; includes it), else the Lisp function (which signals the type error).
;;; %STRUCT-REF / %STRUCT-SET: slot I of a structure with a slot vector;
;;; %STRUCT-IREF / %STRUCT-ISET: of a structure whose slots are fields

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
(defun struct-check-depth-code (o c d)
  "STRUCT-CHECK-CODE for a structure class (local C) whose depth is the constant D."
  (let ((l (new-local)))
    `((:local.get ,o) (:ref.test :instance)
      (:if nil :i32
           ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 0) (:local.tee ,l) (:local.get ,c) :ref.eq
            (:if nil :i32 ((:i32.const 1))
                 ,(inherits-depth-check-code l c d `((:local.get ,o) (:local.get ,c) (:call :struct-typep)))))
           ((:i32.const 0))))))
;; of a structure of a slot vector whose shape (runtime/struct.lisp) gives its depth
(defprim clysm-sys::%struct-ref (x class i shape) :value
  (let ((d (struct-depth-index shape)))
    (if (null d)
        `(,@(funcall (prim-fn (find-primitive 'clysm-sys::%struct-ref 3)) (list x class i)) ,@shape :drop)
        (let ((o (new-local)) (c (new-local)) (k (new-local)))
          `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,k)
            ,@(struct-check-depth-code o c d)
            (:if nil :eqref
                 ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 1)
                  (:local.get ,k) (:ref.cast! :i31) :i31.get_s (:array.get :vector))
                 ((:local.get ,o) (:local.get ,c) (:local.get ,k)
                  ,@(lisp-call-instrs 'clysm-sys::%struct-ref 3))))))))
(defprim clysm-sys::%struct-set (x class i v shape) :value
  (let ((d (struct-depth-index shape)))
    (if (null d)
        `(,@(funcall (prim-fn (find-primitive 'clysm-sys::%struct-set 4)) (list x class i v)) ,@shape :drop)
        (let ((o (new-local)) (c (new-local)) (k (new-local)) (val (new-local)))
          `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,k) ,@v (:local.set ,val)
            ,@(struct-check-depth-code o c d)
            (:if nil :eqref
                 ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 1)
                  (:local.get ,k) (:ref.cast! :i31) :i31.get_s (:local.get ,val) (:array.set :vector)
                  (:local.get ,val))
                 ((:local.get ,o) (:local.get ,c) (:local.get ,k) (:local.get ,val)
                  ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))))))))

(defun struct-iref-index (i)
  "The constant slot index of the code I (a fixnum constant), or NIL."
  (and (= (length i) 2) (consp (first i)) (eq (car (first i)) :i32.const) (eq (second i) :ref.i31)
       (second (first i))))
(defun struct-iref-code (x class k n d &optional (v nil vp))
  "Code reading (or, with V, setting) field slot K (a constant) of the structure X
of CLASS, a structure of N field slots: its own instances are $si<N> (an exact
type test), those of structures including it of a subtype."
  (let ((o (new-local)) (c (new-local)) (val (new-local)) (s (new-local `(:ref :null ,(sinst-type n))))
        (l (new-local)) (slow (gensym "SLOW")) (hit (gensym "HIT")) (done (gensym "SREF")) (ty (sinst-type n)))
    `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@(when vp `(,@v (:local.set ,val)))
      (:block ,done :eqref
        (:block ,slow nil
          (:block ,hit nil
            (:local.get ,o) (:ref.test ,ty) :i32.eqz (:hint 0) (:br_if ,slow)
            (:local.get ,o) (:ref.cast! ,ty) (:local.tee ,s) (:struct.get ,ty 0) (:local.tee ,l) (:local.get ,c)
            :ref.eq (:hint 1) (:br_if ,hit)
            ;; a structure that includes CLASS: by its slot vector, else its layout
            ;; (else the Lisp function: a structure made otherwise, a heap image's)
            ,@(sinst-inherits-check-code s ty c d '((:i32.const 0))) :i32.eqz (:br_if ,slow))
          (:local.get ,s)
          ,@(if vp
                `((:local.get ,val) (:struct.set ,ty ,(sinst-field k)) (:local.get ,val))
                `((:struct.get ,ty ,(sinst-field k))))
          (:br ,done))
        (:local.get ,o) (:local.get ,c) (:i32.const ,k) :ref.i31
        ,@(if vp
              `((:local.get ,val) ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))
              (lisp-call-instrs 'clysm-sys::%struct-ref 3))))))
;; the shape of a structure of field slots (runtime/struct.lisp): its size
;; plus 64 times its depth (the number of structures it includes)
(defun struct-size-index (n)
  (let ((k (struct-iref-index n))) (and k (<= 1 (mod k 64) +struct-inline-max+) (mod k 64))))
(defun struct-depth-index (n)
  (let ((k (struct-iref-index n))) (and k (floor k 64))))

(defun sinst-inherits-check-code (s ty c d fallback)
  "Code pushing 1 when the structure of field slots S (a local of type TY)
includes the class C, at depth D, by the vector of included structures its
constructor put in its slot vector (LAYOUT-INHERITS); FALLBACK (code pushing
an i32) when that is empty (a structure made otherwise: a heap image's)."
  (let ((iv (new-local '(:ref :null :vector))) (n (new-local :i32)))
    `((:local.get ,s) (:struct.get ,ty 1) (:local.tee ,iv) :array.len (:local.tee ,n) (:i32.const ,d) :i32.gt_u
      (:if nil :i32 ((:local.get ,iv) (:i32.const ,d) (:array.get :vector) (:local.get ,c) :ref.eq)
           ((:local.get ,n) :i32.eqz (:if nil :i32 ,fallback ((:i32.const 0))))))))

(defun inherits-depth-check-code (l c d &optional (other '((:i32.const 0))))
  "Code pushing 1 when the layout (local L, not C) includes C, whose index in
the included structures (+CLASS-INHERITS+) is the constant D; OTHER (code
pushing an i32) when the layout is not a structure class (a class defined
with DEFCLASS whose superclasses include a structure class)."
  (let ((li (new-local)) (lv (new-local '(:ref :null :vector))))
    `((:local.get ,l) (:ref.test :instance)
      (:if nil :i32
           ((:local.get ,l) (:ref.cast! :instance) (:struct.get :instance 1) (:i32.const ,+class-inherits-slot+)
            (:array.get :vector) (:local.tee ,li) (:ref.test :vector)
            (:if nil :i32
                 ((:local.get ,li) (:ref.cast! :vector) (:local.tee ,lv) :array.len (:i32.const ,d) :i32.gt_u
                  (:if nil :i32 ((:local.get ,lv) (:i32.const ,d) (:array.get :vector) (:local.get ,c) :ref.eq)
                       ((:i32.const 0))))
                 ,other))
           ,other))))
;; (with an index that is not a constant here: the generic slot access)
(defprim clysm-sys::%struct-iref (x class i n) :value
  (let ((k (struct-iref-index i)) (size (struct-size-index n)))
    (if (and k size (< -1 k size))
        (struct-iref-code x class k size (struct-depth-index n))
        (let ((o (new-local)) (c (new-local)) (kk (new-local)))
          `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,kk) ,@n :drop
            ,@(struct-check-code o c)
            (:if nil :eqref
                 ((:local.get ,o) (:local.get ,kk) (:call :instance-ref))
                 ((:local.get ,o) (:local.get ,c) (:local.get ,kk)
                  ,@(lisp-call-instrs 'clysm-sys::%struct-ref 3))))))))
(defprim clysm-sys::%struct-iset (x class i v n) :value
  (let ((k (struct-iref-index i)) (size (struct-size-index n)))
    (if (and k size (< -1 k size))
        (struct-iref-code x class k size (struct-depth-index n) v)
        (let ((o (new-local)) (c (new-local)) (kk (new-local)) (val (new-local)))
          `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,kk) ,@v (:local.set ,val) ,@n :drop
            ,@(struct-check-code o c)
            (:if nil :eqref
                 ((:local.get ,o) (:local.get ,kk) (:local.get ,val) (:call :instance-set))
                 ((:local.get ,o) (:local.get ,c) (:local.get ,kk) (:local.get ,val)
                  ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))))))))

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
;; slot I of an instance with a slot vector (CLOS objects, conditions,
;; structures with a slot vector)
(defprim clysm-sys::%instance-ref (x i) :value
  `(,@x (:ref.cast :instance) (:struct.get :instance 1) ,@(idx-code i) (:array.get :vector)))
(defprim clysm-sys::%set-instance-ref (x i v) :value
  (let ((tmp (new-local)))
    `(,@x (:ref.cast :instance) (:struct.get :instance 1) ,@(idx-code i) ,@v (:local.tee ,tmp)
      (:array.set :vector) (:local.get ,tmp))))
;; slot I of any instance: in its slot vector, else (a structure whose slots
;; are fields, src/layout.lisp) field I -- for the paths that may see structures
(defprim clysm-sys::%instance-gref (x i) :value `(,@x ,@i (:call :instance-ref)))
(defprim clysm-sys::%set-instance-gref (x i v) :value `(,@x ,@i ,@v (:call :instance-set)))

;;; structures whose slots are fields ($si1 ... $siN, src/layout.lisp)
(defun sinst-dispatch-code (k-local arm default)
  "Helper-body code: a br_table on the i32 local K-LOCAL (0 .. +STRUCT-INLINE-MAX+ - 1)
to the code (FUNCALL ARM J) for J; DEFAULT for any other value."
  (let* ((labels (loop for j below +struct-inline-max+ collect (gensym "SI")))
         (dflt (gensym "SIDEFAULT"))
         (code `((:local.get ,k-local) (:br_table ,@labels ,dflt))))
    (loop for j from 0 for l in labels
          do (setf code `((:block ,l nil ,@code) ,@(funcall arm j))))
    `((:block ,dflt nil ,@code) ,@default)))
;; the number of field slots of an instance (0: not a structure of field slots)
(defun sinst-size-code (lo hi)
  ;; the instance (local 0) is known to be a $si<lo> (or not a $si1 when LO is 0)
  (if (= lo hi)
      `((:i32.const ,lo))
      (let ((mid (ceiling (+ lo hi) 2)))
        `((:local.get 0) (:ref.test ,(sinst-type mid))
          (:if nil :i32 ,(sinst-size-code mid hi) ,(sinst-size-code lo (1- mid)))))))
;; a structure whose slots are fields (with typed slots too)
(defprim clysm-sys::%sinstp (x) :test `(,@x (:ref.test :sfield)))
(defprim clysm-sys::%sinst-size (x) :value `(,@x (:call :sinst-size) :ref.i31))
(defprim clysm-sys::%sinst-make (k) :value `(,@(idx-code k) (:call :sinst-make)))
;; a structure of class LAYOUT with the slots ARGS: its own fields for up to
;; +STRUCT-INLINE-MAX+ slots, else a slot vector
;; a structure of more than +STRUCT-INLINE-MAX+ slots that includes one of
;; field slots (runtime/struct.lisp, :BIG): a $si<max>, its first slots in
;; fields, the others in an overflow vector at the end of its slot vector
;; (after the structures its class includes), so that it is of the Wasm
;; type of every structure it includes
(defprim clysm-sys::%make-big-struct :any :value
  (let* ((vals (rest args)) (n (length vals)) (c (new-local))
         (tmps (loop repeat n collect (new-local))) (m +struct-inline-max+))
    `(,@(first args) (:local.set ,c)
      ,@(loop for v in vals for tmp in tmps append `(,@v (:local.set ,tmp)))
      (:local.get ,c) (:local.get ,c)
      ,@(loop for tmp in (nthcdr m tmps) collect `(:local.get ,tmp)) (:array.new_fixed :vector ,(- n m))
      (:call :big-inherits) (:ref.cast :vector) (:i32.const 0)
      ,@(loop for tmp in tmps repeat m collect `(:local.get ,tmp))
      (:struct.new ,(sinst-type m)))))

(defprim clysm-sys::%make-struct :any :value
  (let ((n (length (rest args))) (c (new-local)))
    (if (<= 1 n +struct-inline-max+)
        `(,@(first args) (:local.tee ,c) (:local.get ,c) (:call :layout-inherits) (:i32.const 0)
          ,@(apply #'append (rest args))
          (:struct.new ,(sinst-type n)))
        `(,@(first args) ,@(apply #'append (rest args)) (:array.new_fixed :vector ,n) (:i32.const 0)
          (:struct.new :instance)))))

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
(defprim clysm-sys::%bsp () :value `(,@(bsp-code) :ref.i31))
(defprim clysm-sys::%unbind-to (sp) :value `(,@sp (:ref.cast :i31) :i31.get_s (:call :unbind-to) ,@(nil-code)))
;; the binding stack itself, for computations that wait (runtime/js.lisp):
;; each has a stack of its own, installed while it runs
(defprim clysm-sys::%bstack () :value (bstack-code))
(defprim clysm-sys::%set-bstack (v) :value `((:global.get :dyn) ,@v (:ref.cast :vector) (:struct.set :dynstate 1) ,@(nil-code)))
(defprim clysm-sys::%set-bsp (n) :value `(,@(set-bsp-code (idx-code n)) ,@(nil-code)))
;; exchange the value of the symbol of each entry of STACK below TOP with the
;; value saved in the entry: from the top down (UP false) when a computation
;; stops running, so that the symbols get their values from outside it and
;; the entries the computation's values; from the bottom up (UP true) when
;; it runs again
(defprim clysm-sys::%swap-bindings (stack top up) :value
  (let ((st (new-local '(:ref :null :vector))) (i (new-local :i32)) (n (new-local :i32))
        (step (new-local :i32)) (sym (new-local '(:ref :null :symbol))) (tmp (new-local :eqref))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@stack (:ref.cast :vector) (:local.set ,st)
      ,@(idx-code top) (:local.set ,n)
      ,@up ,@(nil-code) :ref.eq
      (:if nil nil
           ((:local.get ,n) (:i32.const 2) :i32.sub (:local.set ,i) (:i32.const -2) (:local.set ,step))
           ((:i32.const 0) (:local.set ,i) (:i32.const 2) (:local.set ,step)))
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,i) (:i32.const 0) :i32.lt_s (:br_if ,done)
          (:local.get ,i) (:local.get ,n) :i32.ge_s (:br_if ,done)
          (:local.get ,st) (:local.get ,i) (:array.get :vector) (:ref.cast :symbol) (:local.set ,sym)
          (:local.get ,sym) (:struct.get :symbol ,+sym-value+) (:local.set ,tmp)
          (:local.get ,sym)
          (:local.get ,st) (:local.get ,i) (:i32.const 1) :i32.add (:array.get :vector)
          (:struct.set :symbol ,+sym-value+)
          (:local.get ,st) (:local.get ,i) (:i32.const 1) :i32.add (:local.get ,tmp) (:array.set :vector)
          (:local.get ,i) (:local.get ,step) :i32.add (:local.set ,i)
          (:br ,again)))
      ,@(nil-code))))
;; throw to a SYS:%CATCH (the tag is known to be active): a return with the
;; exit pending
(defprim clysm-sys::%throw (tag value) :value
  (if *fexit*
      `((:i32.const 1) (:global.set :mv-count) ,@(flagged-exit-code tag value) :unreachable)
      `(,@tag ,@value (:i32.const 1) (:global.set :mv-count) (:throw :lisp-throw))))
(defprim clysm-sys::%write-char (c fd) :value
  (let ((tmp (new-local)))
    `(,@fd (:ref.cast :i31) :i31.get_s ,@c (:local.tee ,tmp) (:call :check-char) (:i32.const ,+char-tag+) :i32.sub (:call :host-write-char)
      (:local.get ,tmp))))
(defprim clysm-sys::%read-char () :value `((:call :host-read-char) :ref.i31))
(defprim clysm-sys::%exit (code) :value `(,@code (:ref.cast :i31) :i31.get_s (:call :host-exit) ,@(nil-code)))
(defprim clysm-sys::%apply-vector (f v) :value
  (let ((tmp (new-local '(:ref :null :closure))))
    (checked-call
     `(,@f (:call :coerce-function) (:local.tee ,tmp) :ref.as_non_null ,@v (:ref.cast :vector)
       (:local.get ,tmp) ,@(clo-field-code +clo-entry-n+) (:call_ref :fN)))))
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
      ;; (:apply f spread... list): the spread arguments are consed onto the
      ;; list unless the function is known (CG-APPLY)
      `(:apply ,@(convert-args args env)))))

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

(defmacro defhelper* (name type locals &body body)
  "Like DEFHELPER, with LOCALS evaluated."
  `(progn
     (setf *helpers* (remove ',name *helpers* :key #'first :test #'equal))
     (push (list ',name ',type ,locals
                 (lambda () (loop for x in (list ,@body)
                                  if (and (consp x) (consp (car x))) append x else collect x)))
           *helpers*)
     ',name))

(defvar *imports*
  '((:host-write-char :ii-v "env" "write_char")
    (:host-read-char :v-i "env" "read_char")
    (:host-exit :i-v "env" "exit")
    (:host-identity-hash :x-i "env" "identity_hash")
    ;; heap images (runtime/image.lisp)
    (:host-code-index :fr-i "env" "code_index")
    (:host-module-count :v-i "env" "module_count")
    (:host-module-snap :i-e "env" "module_snap")
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
    (:host-instantiate-module :x-run "env" "instantiate_module")
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
    (:js-await :x-x "js" "await")
    (:js-wrap-async-fn :x-x "js" "wrap_async_fn")
    (:js-host :i-x "js" "host")
    (:js-str-from-mem :i-x "js" "str_from_mem")
    (:js-str-to-mem :x-i "js" "str_to_mem")
    (:js-octets-from-mem :i-x "js" "octets_from_mem")
    (:js-octets-to-mem :x-i "js" "octets_to_mem")
    (:js-read-file-mem :x-i "js" "read_file_mem")
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
  (setf *helpers* (remove-helpers-of-kinds '(:callsym)))
  (loop for k from 0 to +max-fixed-args+
        do (let ((k k))
             (push (list (list :callsym k)
                         (let ((tn (list :callsym-type k)))
                           (unless (gethash tn *extra-types*)
                             (setf (gethash tn *extra-types*) t)
                             (add-type *m* tn `(:func (,@(make-list k :initial-element :eqref) (:ref :symbol))
                                                      (:eqref))))
                           tn)
                         '((clo (:ref :null :closure)))
                         (lambda ()
                           (let ((l (gensym)) (ok (gensym)))
                             `((:block ,l (:ref :closure)
                                 (:local.get ,k) (:struct.get :symbol ,+sym-function+) (:br_on_non_null ,l)
                                 (:local.get ,k) (:call :undefined-function))
                               (:local.set clo)
                               (:block ,ok :eqref
                                 (:local.get clo) (:ref.as_non_null)
                                 ,@(loop for i below k collect `(:local.get ,i))
                                 (:local.get clo) ,@(clo-field-code k)
                                 (:call_ref ,(fixed-entry-type k))
                                 (:br_on_non_null ,ok)
                                 ;; null: an exit pending (thrown: the callers of
                                 ;; the helpers do not look), or an unbound marker
                                 (:call :exit-pending-p)
                                 (:if nil nil ((:call :throw-pending-exit)))
                                 (:ref.null :eq))))))
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

;;; The slow path of an interrupt check (POLL-CODE): every
;;; +POLL-INTERVAL+ checks, SYSTEM::%INTERRUPT-POLL is called.
(defconstant +poll-interval+ 100000)
;; the number of checks until the next call (fewer while threads run)
(defprim clysm-sys::%set-poll-count (n) :value
  `(,@(idx-code n) (:global.set :poll-count) ,@(nil-code)))
(defhelper :poll-slow :v-v ()
  `(:i32.const ,+poll-interval+) '(:global.set :poll-count)
  `(:global.get ,(sym-global 'clysm-sys::%interrupt-poll)) '(:call (:callsym 0)) :drop)

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
  '(:local.get 0) '(:local.get 1) '(:return_call :eql-gen))

;;; two different heap objects that are not conses or symbols
(defhelper :eql-gen :ee-i ((wa :i64) (wb :i64))
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
  '(:local.get 0) '(:call :complexp) :i32.or :i32.eqz
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
                 ;; within 63 bits: one digit, or two with the top bit clear
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) (:call :mag-len)
                 (:i32.const 2) :i32.lt_u (:if nil nil ((:i32.const 1) :return))
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) (:call :mag-len) (:i32.const 2) :i32.eq
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) (:ref.cast! :limbs) (:i32.const 1)
                 (:array.get :limbs) (:i32.const 0) :i32.ge_s :i32.and :return))
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
             (:local.get ,param) (:call :small-int-p) :i32.eqz (:if nil nil (,@(nil-code) :return))
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

;;; EVENP of an integer: a fixnum or an integer within 64 bits inline, the
;;; Lisp function (bignums, type errors) otherwise
(defhelper :evenp :e-i ((v :i32) (w :i64))
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee v)
                 (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                 (:if nil nil ((:local.get v) (:i32.const 1) :i32.and :i32.eqz :return))))
  '(:local.get 0) '(:ref.test :bignum)
  '(:if nil nil ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee w) :i64.eqz :i32.eqz
                 (:if nil nil ((:local.get w) :i32.wrap_i64 (:i32.const 1) :i32.and :i32.eqz :return))))
  '(:local.get 0) `(:global.get ,(sym-global 'evenp)) '(:call (:callsym 1))
  (car (nil-code)) :ref.eq :i32.eqz)
(defprim evenp (a) :test `(,@a (:call :evenp)))
(defprim oddp (a) :test `(,@a (:call :evenp) :i32.eqz))
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

;;; MEMBER / ASSOC / RASSOC with EQL of an item that EQ compares (a fixnum,
;;; a character, a symbol, a cons, an instance...: not a boxed number) on a
;;; proper list: an EQ loop in Wasm.  Other items, and a list that is not
;;; proper (an error to signal), take the Lisp function.
(defun eq-item-code (x slow)
  "Branch to SLOW unless the eqref local X is compared by EQ under EQL."
  `((:local.get ,x) (:ref.test :i31) (:local.get ,x) (:ref.test :symbol) :i32.or
    (:local.get ,x) (:ref.test :cons) :i32.or (:local.get ,x) (:ref.test :instance) :i32.or
    (:local.get ,x) (:ref.test :string) :i32.or (:local.get ,x) (:ref.test :vector) :i32.or
    :i32.eqz (:br_if ,slow)))
(defhelper :member-eq-loop :ee-e ((l :eqref))
  `(:block slow nil
     ,@(eq-item-code 0 'slow)
     (:local.get 1) (:local.set l)
     (:loop again nil
       (:local.get l) (:ref.test :cons)
       (:if nil nil
            ((:local.get l) (:ref.cast! :cons) (:struct.get :cons 0) (:local.get 0) :ref.eq
             (:if nil nil ((:local.get l) :return))
             (:local.get l) (:ref.cast! :cons) (:struct.get :cons 1) (:local.set l)
             (:br again)))
       (:local.get l) ,@(nil-code) :ref.eq (:if nil nil (,@(nil-code) :return))))
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%member-eql)) '(:call (:callsym 2)))
(defprim clysm-sys::%member-eql (x l) :value `(,@x ,@l (:call :member-eq-loop)))
(defhelper :assoc-eq-loop :eei-e ((l :eqref) (e :eqref))
  ;; the third parameter: 1 for RASSOC
  `(:block slow nil
     ,@(eq-item-code 0 'slow)
     (:local.get 1) (:local.set l)
     (:loop again nil
       (:local.get l) (:ref.test :cons)
       (:if nil nil
            ((:local.get l) (:ref.cast! :cons) (:struct.get :cons 0) (:local.tee e) (:ref.test :cons)
             (:if nil nil
                  ((:local.get 2)
                   (:if nil :eqref ((:local.get e) (:ref.cast! :cons) (:struct.get :cons 1))
                        ((:local.get e) (:ref.cast! :cons) (:struct.get :cons 0)))
                   (:local.get 0) :ref.eq
                   (:if nil nil ((:local.get e) :return))))
             ;; an entry that is neither a cons nor NIL: the error, from Lisp
             (:local.get e) ,@(nil-code) :ref.eq (:local.get e) (:ref.test :cons) :i32.or :i32.eqz (:br_if slow)
             (:local.get l) (:ref.cast! :cons) (:struct.get :cons 1) (:local.set l)
             (:br again)))
       (:local.get l) ,@(nil-code) :ref.eq (:if nil nil (,@(nil-code) :return))))
  '(:local.get 2)
  `(:if nil :eqref
        ((:local.get 0) (:local.get 1) (:global.get ,(sym-global 'clysm-sys::%rassoc-eql)) (:call (:callsym 2)))
        ((:local.get 0) (:local.get 1) (:global.get ,(sym-global 'clysm-sys::%assoc-eql)) (:call (:callsym 2)))))
(defprim clysm-sys::%assoc-eql (x l) :value `(,@x ,@l (:i32.const 0) (:call :assoc-eq-loop)))
(defprim clysm-sys::%rassoc-eql (x l) :value `(,@x ,@l (:i32.const 1) (:call :assoc-eq-loop)))



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
     (:local.get n) (:i32.const 2) :i32.gt_u (:br_if big)
     (:local.get m) (:i32.const 0) (:array.get :limbs) :i64.extend_i32_u (:local.set w)
     (:local.get n) (:i32.const 1) :i32.gt_u
     (:if nil nil ((:local.get m) (:i32.const 1) (:array.get :limbs) :i64.extend_i32_u (:i64.const ,+limb-bits+) :i64.shl
                   (:local.get w) :i64.or (:local.set w)))
     (:local.get w) (:i64.const 0) :i64.lt_s
     (:if nil nil
          (;; 2^63 or more: -2^63 is the one more value an i64 holds
           (:local.get w) (:i64.const ,(- (expt 2 63))) :i64.eq (:local.get 1) :i32.and :i32.eqz (:br_if big)
           (:i64.const ,(- (expt 2 63))) (:call :i64-int) :return))
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
  ;; (a fixnum's magnitude is one digit)
  '(:local.get v) '(:array.new_fixed :limbs 1))
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
;;; |M| + W / |M| - W for a magnitude M and 0 <= W < 2^64 (unsigned; for the
;;; difference |M| >= W): no limbs are made for W.  The sum has one more
;;; digit only when it carries out.
(defhelper :mag-addsub-u64 :eli-e ((m (:ref :null :limbs)) (r (:ref :null :limbs)) (r2 (:ref :null :limbs))
                                   (n :i32) (i :i32) (w :i64) (tt :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee m) '(:call :mag-len) '(:local.tee n)
  '(:array.new_default :limbs) '(:local.set r)
  '(:local.get 1) '(:local.set w)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get w) :i64.eqz
       (:if nil nil
            (;; nothing left to add: the rest of M as it is
             (:local.get r) (:local.get i) (:local.get m) (:local.get i) (:local.get n) (:local.get i) :i32.sub
             (:array.copy :limbs :limbs)
             (:br done)))
       (:local.get m) (:local.get i) (:array.get :limbs) :i64.extend_i32_u
       (:i64.const 0) (:local.get w) (:i64.const ,+limb-mask+) :i64.and :i64.sub
       (:local.get w) (:i64.const ,+limb-mask+) :i64.and
       (:local.get 2) :select :i64.add (:local.set tt)
       (:local.get r) (:local.get i) (:local.get tt) :i32.wrap_i64 (:array.set :limbs)
       ;; the carry (0 / 1) or the borrow (the sign bit) with the rest of W
       (:local.get w) (:i64.const ,+limb-bits+) :i64.shr_u
       (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:i64.const 1) :i64.and :i64.add
       (:local.set w)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get w) :i64.eqz '(:if nil nil ((:local.get r) :ref.as_non_null :return))
  ;; a carry out of the top digit (W beyond M's digits for a sum)
  '(:local.get n) '(:i32.const 2) :i32.add '(:array.new_default :limbs) '(:local.set r2)
  '(:local.get r2) '(:i32.const 0) '(:local.get r) '(:i32.const 0) '(:local.get n) '(:array.copy :limbs :limbs)
  '(:local.get r2) '(:local.get n) '(:local.get w) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r2) '(:local.get n) '(:i32.const 1) :i32.add '(:local.get w) '(:i64.const 32) :i64.shr_u :i32.wrap_i64
  '(:array.set :limbs)
  '(:local.get r2) :ref.as_non_null)

(defhelper :big-add :eei-e ((sa :i32) (sb :i32) (ma :eqref) (mb :eqref) (c :i32) (w :i64) (v :i32) (k :i32))
  ;; a bignum of digits and an integer within 64 bits (as the second
  ;; operand, or the first in a sum): the small one is not made digits
  `(:block general nil
     (:local.get 0) (:ref.test :bignum) (:local.get 1) (:ref.test :bignum) :i32.and
     (:local.get 2) :i32.eqz :i32.and
     (:if nil nil
          (;; a sum with the small one first: the other way round
           (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) :i64.eqz :i32.eqz
           (:local.get 1) (:ref.cast! :bignum) (:struct.get :bignum 2) :i64.eqz :i32.and
           (:if nil nil ((:local.get 1) (:local.get 0) (:i32.const 0) (:return_call :big-add)))))
     (:local.get 0) (:ref.test :bignum) :i32.eqz (:br_if general)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) :i64.eqz :i32.eqz (:br_if general)
     ,@(num-decode-code 1 'w nil 'k 'general nil)
     ;; B's sign for the operation; |B| as an unsigned i64
     (:local.get w) (:i64.const 0) :i64.lt_s (:local.get 2) :i32.xor (:local.set sb)
     (:local.get w) (:i64.const 0) :i64.lt_s
     (:if nil nil ((:i64.const 0) (:local.get w) :i64.sub (:local.set w)))
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 0) (:local.set sa)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1)
     (:local.get w)
     (:local.get sa) (:local.get sb) :i32.ne (:call :mag-addsub-u64)
     (:local.get sa) (:call :make-int) :return)
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
  ;; a fixnum (below the digit base): one pass
  (loop for (p q) in '((1 0) (0 1))
        collect `(:block notfx nil
                   (:local.get ,p) (:ref.test :i31) :i32.eqz (:br_if notfx)
                   (:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee v) :i32.eqz
                   (:if nil nil ((:i32.const 0) :ref.i31 :return))
                   (:local.get v) (:i32.const 0) :i32.lt_s
                   (:if nil nil ((:i32.const 0) (:local.get v) :i32.sub (:local.set v)))
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
                             ;; (the carry goes on past a digit that wraps to 0)
                             ((:local.get ,dst) (:i32.const -1) :i32.xor (:local.get ,carry) :i32.add
                              (:local.tee ,dst) :i32.eqz (:local.get ,carry) :i32.and (:local.set ,carry)))))
       (:block o2 nil (:block o1 nil (:block o0 nil
          (:local.get 2) (:br_table o0 o1 o2))
          (:local.get x) (:local.get y) :i32.and (:local.set d) (:br o2))
          (:local.get x) (:local.get y) :i32.or (:local.set d) (:br o2))
       (:local.get 2) (:i32.const 2) :i32.eq (:if nil nil ((:local.get x) (:local.get y) :i32.xor (:local.set d)))
       (:local.get sr)
       (:if nil nil
            ((:local.get d) (:i32.const -1) :i32.xor (:local.get cr) :i32.add
             (:local.tee d) :i32.eqz (:local.get cr) :i32.and (:local.set cr)))
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
(defprim clysm-sys::%mag-len (m) :value `(,@m (:call :mag-len) :ref.i31))
;;; K < 30 bits of magnitude M from bit START (0 beyond its digits)
(defhelper :mag-bit-field :eii-e ((m (:ref :null :limbs)) (j :i32) (n :i32) (w :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee m) :array.len '(:local.set n)
  '(:local.get 1) `(:i32.const ,+limb-bits+) :i32.div_u '(:local.tee j) '(:local.get n) :i32.lt_u
  '(:if nil nil ((:local.get m) (:local.get j) (:array.get :limbs) :i64.extend_i32_u (:local.set w)))
  '(:local.get j) '(:i32.const 1) :i32.add '(:local.get n) :i32.lt_u
  `(:if nil nil ((:local.get m) (:local.get j) (:i32.const 1) :i32.add (:array.get :limbs) :i64.extend_i32_u
                 (:i64.const ,+limb-bits+) :i64.shl (:local.get w) :i64.or (:local.set w)))
  '(:local.get w) `(:local.get 1) `(:i32.const ,+limb-bits+) :i32.rem_u :i64.extend_i32_u :i64.shr_u :i32.wrap_i64
  '(:i32.const 1) '(:local.get 2) :i32.shl '(:i32.const 1) :i32.sub :i32.and :ref.i31)
(defprim clysm-sys::%mag-bit-field (m start k) :value `(,@m ,@(idx-code start) ,@(idx-code k) (:call :mag-bit-field)))
;;; an integer within 64 bits as a double (exact below 2^53)
(defprim clysm-sys::%int64-to-double (x) :value `(,@x (:call :int-i64) :f64.convert_i64_s (:struct.new :double)))
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
       ;; the two digits as an i64, shifted
       (:local.get i) (:i32.const 1) :i32.add (:local.get k) :i32.lt_u
       (:if nil :i64
            ((:local.get a) (:local.get i) (:local.get dg) :i32.add (:i32.const 1) :i32.add (:array.get :limbs)
             :i64.extend_i32_u (:i64.const ,+limb-bits+) :i64.shl)
            ((:i64.const 0)))
       (:local.get a) (:local.get i) (:local.get dg) :i32.add (:array.get :limbs) :i64.extend_i32_u :i64.or
       (:local.get s) :i64.extend_i32_u :i64.shr_u :i32.wrap_i64 (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-shl (a n) :value `(,@a ,@n (:call :mag-shl-n)))
(defprim clysm-sys::%mag-shr (a n) :value `(,@a ,@n (:call :mag-shr-n)))

(defun mul-acc-code (ai bi)
  "Add the product of the digits AI and BI (i64 expressions) to the column
sums LO (low halves) and HI (high halves)."
  `(,@ai ,@bi :i64.mul
    (:local.tee p) (:i64.const ,+limb-mask+) :i64.and (:local.get lo) :i64.add (:local.set lo)
    (:local.get p) (:i64.const ,+limb-bits+) :i64.shr_u (:local.get hi) :i64.add (:local.set hi)))
(defun limb-code (arr idx)
  `((:local.get ,arr) ,@idx (:array.get :limbs) :i64.extend_i32_u))
(defhelper :mag-mul :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (k :i32) (i :i32) (iend :i32) (lo :i64) (hi :i64) (p :i64)
                           (c :i64))
  ;; column by column (product scanning): the products of a column are summed
  ;; in two accumulators, of their low and of their high halves, without a
  ;; carry from one product to the next; C carries into the next column.  A
  ;; square takes each product of two different digits once, doubled.
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) :i32.eqz '(:local.get lb) :i32.eqz :i32.or
  '(:if nil nil ((:i32.const 0) (:array.new_default :limbs) :return))
  '(:local.get la) '(:local.get lb) :i32.add '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop outer nil
       (:local.get k) (:local.get la) (:local.get lb) :i32.add (:i32.const 1) :i32.sub :i32.ge_u (:br_if done)
       ;; i from max(0, k - lb + 1) to min(k, la - 1)
       (:local.get k) (:local.get lb) :i32.sub (:i32.const 1) :i32.add (:local.tee i)
       (:i32.const 0) (:local.get i) (:i32.const 0) :i32.gt_s :select (:local.set i)
       (:local.get k) (:local.get la) (:i32.const 1) :i32.sub (:local.tee iend)
       (:local.get k) (:local.get iend) :i32.lt_s :select (:local.set iend)
       (:i64.const 0) (:local.set lo) (:i64.const 0) (:local.set hi)
       (:local.get a) (:local.get b) :ref.eq
       (:if nil nil
            (;; a square: the products a_i a_(k-i) for i < k - i, doubled, then a_(k/2)^2
             (:local.get k) (:i32.const 1) :i32.sub (:i32.const 1) :i32.shr_s (:local.set iend)
             (:block sdone nil
               (:loop sagain nil
                 (:local.get i) (:local.get iend) :i32.gt_s (:br_if sdone)
                 ,@(mul-acc-code (limb-code 'a '((:local.get i)))
                                 (limb-code 'a '((:local.get k) (:local.get i) :i32.sub)))
                 (:local.get i) (:i32.const 1) :i32.add (:local.set i)
                 (:br sagain)))
             (:local.get lo) (:i32.const 1) :i64.extend_i32_u :i64.shl (:local.set lo)
             (:local.get hi) (:i32.const 1) :i64.extend_i32_u :i64.shl (:local.set hi)
             (:local.get k) (:i32.const 1) :i32.and :i32.eqz
             (:if nil nil
                  ,(mul-acc-code (limb-code 'a '((:local.get k) (:i32.const 1) :i32.shr_u))
                                 (limb-code 'a '((:local.get k) (:i32.const 1) :i32.shr_u))))
             (:local.get iend) (:i32.const 2) :i32.add (:local.set i)))
       ;; two products a step
       (:block idone nil
         (:loop inner nil
           (:local.get i) (:local.get iend) :i32.ge_s (:br_if idone)
           ,@(mul-acc-code (limb-code 'a '((:local.get i))) (limb-code 'b '((:local.get k) (:local.get i) :i32.sub)))
           ,@(mul-acc-code (limb-code 'a '((:local.get i) (:i32.const 1) :i32.add))
                           (limb-code 'b '((:local.get k) (:local.get i) :i32.sub (:i32.const 1) :i32.sub)))
           (:local.get i) (:i32.const 2) :i32.add (:local.set i)
           (:br inner)))
       (:local.get i) (:local.get iend) :i32.eq
       (:if nil nil ,(mul-acc-code (limb-code 'a '((:local.get i))) (limb-code 'b '((:local.get k) (:local.get i) :i32.sub))))
       (:local.get lo) (:local.get c) :i64.add (:local.set lo)
       (:local.get r) (:local.get k) (:local.get lo) :i32.wrap_i64 (:array.set :limbs)
       (:local.get lo) (:i64.const ,+limb-bits+) :i64.shr_u (:local.get hi) :i64.add (:local.set c)
       (:local.get k) (:i32.const 1) :i32.add (:local.set k)
       (:br outer)))
  '(:local.get r) '(:local.get k) '(:local.get c) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-mul-fast (a b) :value `(,@a ,@b (:call :mag-mul)))

;;; A times one digit 0 < D < 2^28: the trimmed product
(defhelper :mag-mul1 :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (d :i64)
                            (carry :i64) (tt :i64) (m :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a)
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) :i32.eqz
  '(:if nil nil ((:i32.const 0) (:array.new_default :limbs) :return))
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s :i64.extend_i32_s '(:local.set d)
  ;; one more limb only when the top limb may carry out: (top + 1) * d > 2^28
  '(:local.get a) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :limbs) :i64.extend_i32_u '(:i64.const 1) :i64.add
  '(:local.get d) :i64.mul `(:i64.const ,(expt 2 +limb-bits+)) :i64.gt_u
  '(:local.get n) :i32.add '(:local.tee m) '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get d) :i64.mul
       (:local.get carry) :i64.add (:local.set tt)
       (:local.get r) (:local.get i) (:local.get tt) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get m) '(:local.get n) :i32.eq '(:if nil nil ((:local.get r) :ref.as_non_null :return))
  '(:local.get carry) :i64.eqz
  `(:if nil nil
        (;; no carry out after all: the first N limbs
         (:local.get n) (:array.new_default :limbs) (:local.set a)
         (:local.get a) (:i32.const 0) (:local.get r) (:i32.const 0) (:local.get n) (:array.copy :limbs :limbs)
         (:local.get a) :ref.as_non_null :return))
  '(:local.get r) '(:local.get n) '(:local.get carry) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-mul-digit (a d) :value `(,@a ,@d (:call :mag-mul1)))

;;; a + b (magnitudes): of the length of the longer one, one more limb only
;;; when the sum carries out
(defhelper :mag-add :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (i :i32) (s :i64) (r2 (:ref :null :limbs)))
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) :i32.ge_u
  '(:if nil nil
        ((:local.get 0) (:ref.cast! :limbs) (:local.set a) (:local.get 1) (:ref.cast! :limbs) (:local.set b))
        ((:local.get 1) (:ref.cast! :limbs) (:local.set a) (:local.get 0) (:ref.cast! :limbs) (:local.set b)
         (:local.get la) (:local.set i) (:local.get lb) (:local.set la) (:local.get i) (:local.set lb)
         (:i32.const 0) (:local.set i)))
  '(:local.get la) '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get lb) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u
       (:local.get b) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.add
       (:local.get s) :i64.add (:local.set s)
       (:local.get r) (:local.get i) (:local.get s) :i32.wrap_i64 (:array.set :limbs)
       (:local.get s) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set s)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  `(:block done2 nil
     (:loop again2 nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done2)
       (:local.get s) :i64.eqz
       (:if nil nil
            (;; no carry left: the rest of A as it is
             (:local.get r) (:local.get i) (:local.get a) (:local.get i) (:local.get la) (:local.get i) :i32.sub
             (:array.copy :limbs :limbs)
             (:br done2)))
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get s) :i64.add (:local.set s)
       (:local.get r) (:local.get i) (:local.get s) :i32.wrap_i64 (:array.set :limbs)
       (:local.get s) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set s)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again2)))
  '(:local.get s) :i64.eqz '(:if nil nil ((:local.get r) :ref.as_non_null :return))
  '(:local.get la) '(:i32.const 1) :i32.add '(:array.new_default :limbs) '(:local.set r2)
  '(:local.get r2) '(:i32.const 0) '(:local.get r) '(:i32.const 0) '(:local.get la) '(:array.copy :limbs :limbs)
  '(:local.get r2) '(:local.get la) '(:i32.const 1) '(:array.set :limbs)
  '(:local.get r2) :ref.as_non_null)
(defprim clysm-sys::%mag-add-fast (a b) :value `(,@a ,@b (:call :mag-add)))

;;; a - b (magnitudes, a >= b)
(defhelper :mag-sub :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (i :i32) (d :i64) (borrow :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get lb) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u
       (:local.get b) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.sub
       (:local.get borrow) :i64.sub (:local.tee d)
       (:i64.const 63) :i64.shr_u (:local.set borrow)
       (:local.get r) (:local.get i) (:local.get d) :i32.wrap_i64 (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  `(:block done2 nil
     (:loop again2 nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done2)
       (:local.get borrow) :i64.eqz
       (:if nil nil
            ((:local.get r) (:local.get i) (:local.get a) (:local.get i) (:local.get la) (:local.get i) :i32.sub
             (:array.copy :limbs :limbs)
             (:br done2)))
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get borrow) :i64.sub (:local.tee d)
       (:i64.const 63) :i64.shr_u (:local.set borrow)
       (:local.get r) (:local.get i) (:local.get d) :i32.wrap_i64 (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again2)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-sub-fast (a b) :value `(,@a ,@b (:call :mag-sub)))

;;; A divided by the one digit D of the magnitude DM (not 0): the quotient
;;; magnitude, the remainder (an integer) as the second value
(defhelper :mag-divmod-small :ee-e ((a (:ref :null :limbs)) (q (:ref :null :limbs)) (n :i32) (d :i64)
                                    (rem :i64) (cur :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a)
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) '(:array.new_default :limbs) '(:local.set q)
  '(:local.get 1) '(:ref.cast! :limbs) '(:i32.const 0) '(:array.get :limbs) :i64.extend_i32_u '(:local.set d)
  `(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
       (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
       (:local.get a) (:local.get n) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.set cur)
       (:local.get q) (:local.get n) (:local.get cur) (:local.get d) :i64.div_u :i32.wrap_i64 (:array.set :limbs)
       (:local.get cur) (:local.get d) :i64.rem_u (:local.set rem)
       (:br again)))
  '(:global.get :mv-buf) '(:i32.const 0) '(:local.get rem) '(:call :i64-int) '(:array.set :vector)
  '(:i32.const 2) '(:global.set :mv-count)
  '(:local.get q) :ref.as_non_null)
(defprim clysm-sys::%mag-divmod-small (a dm) :mv `(,@a ,@dm (:call :mag-divmod-small)))

;;; The first N limbs of a magnitude divided in place by 10^8 / by a fixnum
;;; 0 < D: the remainder (for the conversion to a string, chunk by
;;; chunk; a division by the constant is a multiplication for the engine)
(defhelper :mag-div-1e8 :ei-e ((a (:ref :null :limbs)) (i :i32) (rem :i64) (cur :i64) (q :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:local.set i)
  `(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.tee cur)
       (:i64.const 100000000) :i64.div_u (:local.set q)
       (:local.get a) (:local.get i) (:local.get q) :i32.wrap_i64 (:array.set :limbs)
       (:local.get cur) (:local.get q) (:i64.const 100000000) :i64.mul :i64.sub (:local.set rem)
       (:br again)))
  '(:local.get rem) :i32.wrap_i64 :ref.i31)
(defhelper :mag-div-small :eii-e ((a (:ref :null :limbs)) (i :i32) (d :i64) (rem :i64) (cur :i64) (q :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:local.set i)
  '(:local.get 2) :i64.extend_i32_u '(:local.set d)
  `(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.tee cur)
       (:local.get d) :i64.div_u (:local.set q)
       (:local.get a) (:local.get i) (:local.get q) :i32.wrap_i64 (:array.set :limbs)
       (:local.get cur) (:local.get q) (:local.get d) :i64.mul :i64.sub (:local.set rem)
       (:br again)))
  '(:local.get rem) :i32.wrap_i64 :ref.i31)
(defprim clysm-sys::%mag-div-1e8 (m n) :value `(,@m ,@(idx-code n) (:call :mag-div-1e8)))
(defprim clysm-sys::%mag-div-small (m n d) :value `(,@m ,@(idx-code n) ,@(idx-code d) (:call :mag-div-small)))

;;; magnitude shifts by 0 <= S < 32 bits (left: one more limb)
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
       (:local.get i) (:i32.const 1) :i32.add (:local.get n) :i32.lt_u
       (:if nil :i64
            ((:local.get a) (:local.get i) (:i32.const 1) :i32.add (:array.get :limbs)
             :i64.extend_i32_u (:i64.const ,+limb-bits+) :i64.shl)
            ((:i64.const 0)))
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or
       (:local.get s) :i64.extend_i32_u :i64.shr_u :i32.wrap_i64 (:array.set :limbs)
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
           ;; (no branch: the borrow is the sign bit, the digit the low bits)
           (:i64.const 63) :i64.shr_u (:local.set borrow)
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

;;; The remainder of truncating integer A by integer B when |B| < 2^32 (not
;;; 0) and A is a bignum, digit by digit without a quotient; NIL otherwise.
(defhelper :int-rem-small :ee-e ((v :i32) (d :i64) (ka :i32) (m (:ref :null :limbs)) (n :i32) (rem :i64))
  `(:block fail nil
     ,@(num-decode-code 1 'd nil 'ka 'fail nil)
     (:local.get d) (:i64.const 0) :i64.lt_s (:if nil nil ((:i64.const 0) (:local.get d) :i64.sub (:local.set d)))
     (:local.get d) :i64.eqz (:br_if fail)
     (:local.get d) (:i64.const ,(expt 2 +limb-bits+)) :i64.ge_u (:br_if fail)
     (:local.get 0) (:ref.test :bignum) :i32.eqz (:br_if fail)
     (:local.get 0) (:call :int-limbs) (:ref.cast! :limbs) (:local.tee m) :array.len (:local.set n)
     (:block done nil
       (:loop again nil
         (:local.get n) :i32.eqz (:br_if done)
         (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
         (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
         (:local.get m) (:local.get n) (:array.get :limbs) :i64.extend_i32_u :i64.or
         (:local.get d) :i64.rem_u (:local.set rem)
         (:br again)))
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 0)
     (:if nil :i64 ((:i64.const 0) (:local.get rem) :i64.sub) ((:local.get rem)))
     (:call :i64-int) :return)
  (car (nil-code)))
(defprim clysm-sys::%int-rem-small (a b) :value `(,@a ,@b (:call :int-rem-small)))

;;; Lehmer's gcd (Knuth 4.5.2, Algorithm L; the form of CPython's
;;; _PyLong_GCD): integers A >= B >= 0 are reduced, about a digit of each at
;;; a time, by the quotients the leading 60 bits give, while those are
;;; certain (the cofactors stay below 2^30, their products with digits below
;;; 2^62);
;;; the result (values A' B') has the gcd of A and B.  It stops when A is
;;; within two digits, B is zero or the leading bits give no quotient (the
;;; caller then takes a remainder).  Work arrays of the length of A, the
;;; lengths kept in LA and LB (digits above them are not looked at).
(defun lehmer-trim-code (arr len)
  `((:block tdone nil
      (:loop tagain nil
        (:local.get ,len) :i32.eqz (:br_if tdone)
        (:local.get ,arr) (:local.get ,len) (:i32.const 1) :i32.sub (:array.get :limbs) (:br_if tdone)
        (:local.get ,len) (:i32.const 1) :i32.sub (:local.set ,len)
        (:br tagain)))))
(defun lehmer-digit-code (arr idx len)
  "The digit IDX (an i32 expression) of ARR as an i64, 0 at or above LEN."
  `(,@idx (:local.get ,len) :i32.lt_u
    (:if nil :i64 ((:local.get ,arr) ,@idx (:array.get :limbs) :i64.extend_i32_u) ((:i64.const 0)))))
(defun lehmer-top-code (arr len dst)
  "The bits of ARR (of length LEN) at the places of the leading 60 bits of A
\(LA digits, NB bits in the top one) into DST."
  `(,@(lehmer-digit-code arr '((:local.get la) (:i32.const 1) :i32.sub) len)
    (:i64.const 32) :i64.shl
    ,@(lehmer-digit-code arr '((:local.get la) (:i32.const 2) :i32.sub) len) :i64.or (:local.set w)
    (:local.get nb) (:i64.const 28) :i64.le_u
    (:if nil :i64
         ((:local.get w) (:i64.const 28) (:local.get nb) :i64.sub :i64.shl
          ,@(lehmer-digit-code arr '((:local.get la) (:i32.const 3) :i32.sub) len)
          (:local.get nb) (:i64.const 4) :i64.add :i64.shr_u :i64.or)
         ((:local.get w) (:local.get nb) (:i64.const 28) :i64.sub :i64.shr_u))
    (:local.set ,dst)))
(defhelper :lehmer :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (c (:ref :null :limbs))
                          (d (:ref :null :limbs)) (tmp (:ref :null :limbs))
                          (la :i32) (lb :i32) (i :i32) (k :i32) (nb :i64)
                          (x :i64) (y :i64) (q :i64) (s :i64) (tt :i64)
                          (ca :i64) (cb :i64) (cc :i64) (cd :i64) (ec :i64) (ed :i64) (ai :i64) (bi :i64)
                          (w :i64))
  (let ((l +limb-bits+))
    (list
     '(:local.get 0) '(:call :int-limbs) '(:ref.cast! :limbs) '(:local.tee tmp) '(:call :mag-len) '(:local.tee la)
     '(:array.new_default :limbs) '(:local.set a)
     '(:local.get a) '(:i32.const 0) '(:local.get tmp) '(:i32.const 0) '(:local.get la) '(:array.copy :limbs :limbs)
     '(:local.get 1) '(:call :int-limbs) '(:ref.cast! :limbs) '(:local.tee tmp) '(:call :mag-len) '(:local.set lb)
     '(:local.get la) '(:array.new_default :limbs) '(:local.set b)
     '(:local.get b) '(:i32.const 0) '(:local.get tmp) '(:i32.const 0) '(:local.get lb) '(:array.copy :limbs :limbs)
     '(:local.get la) '(:array.new_default :limbs) '(:local.set c)
     '(:local.get la) '(:array.new_default :limbs) '(:local.set d)
     `(:block exit nil
        (:loop outer nil
          (:local.get la) (:i32.const 2) :i32.le_u (:br_if exit)
          (:local.get lb) :i32.eqz (:br_if exit)
          ;; the leading 60 bits of A in X, the bits of B at the same places in Y
          (:i64.const 32) (:local.get a) (:local.get la) (:i32.const 1) :i32.sub (:array.get :limbs) :i32.clz
          :i64.extend_i32_u :i64.sub (:local.set nb)
          ,@(lehmer-top-code 'a 'la 'x)
          ,@(lehmer-top-code 'b 'lb 'y)
          (:i64.const 1) (:local.set ca) (:i64.const 0) (:local.set cb)
          (:i64.const 0) (:local.set cc) (:i64.const 1) (:local.set cd) (:i32.const 0) (:local.set k)
          (:block idone nil
            (:loop iagain nil
              (:local.get y) (:local.get cc) :i64.sub :i64.eqz (:br_if idone)
              (:local.get x) (:local.get ca) :i64.add (:i64.const 1) :i64.sub
              (:local.get y) (:local.get cc) :i64.sub :i64.div_s (:local.set q)
              (:local.get cb) (:local.get q) (:local.get cd) :i64.mul :i64.add (:local.set s)
              (:local.get x) (:local.get q) (:local.get y) :i64.mul :i64.sub (:local.set tt)
              (:local.get s) (:local.get tt) :i64.gt_s (:br_if idone)
              (:local.get y) (:local.set x) (:local.get tt) (:local.set y)
              (:local.get ca) (:local.get q) (:local.get cc) :i64.mul :i64.add (:local.set tt)
              (:local.get cd) (:local.set ca) (:local.get cc) (:local.set cb)
              (:local.get s) (:local.set cc) (:local.get tt) (:local.set cd)
              (:local.get k) (:i32.const 1) :i32.add (:local.set k)
              (:br iagain)))
          (:local.get k) :i32.eqz (:br_if exit)
          ;; A, B := A*a - B*b, D*b - C*a (for an odd number of steps: A*b - B*a, D*a - C*b)
          (:local.get k) (:i32.const 1) :i32.and
          (:if nil nil
               ((:i64.const 0) (:local.get ca) :i64.sub (:local.set tt)
                (:i64.const 0) (:local.get cb) :i64.sub (:local.set ca) (:local.get tt) (:local.set cb)
                (:i64.const 0) (:local.get cc) :i64.sub (:local.set tt)
                (:i64.const 0) (:local.get cd) :i64.sub (:local.set cc) (:local.get tt) (:local.set cd)))
          (:i64.const 0) (:local.set ec) (:i64.const 0) (:local.set ed) (:i32.const 0) (:local.set i)
          (:block adone nil
            (:loop aagain nil
              (:local.get i) (:local.get la) :i32.ge_u (:br_if adone)
              (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.set ai)
              ,@(lehmer-digit-code 'b '((:local.get i)) 'lb) (:local.set bi)
              (:local.get ec) (:local.get ca) (:local.get ai) :i64.mul :i64.add
              (:local.get cb) (:local.get bi) :i64.mul :i64.sub (:local.set ec)
              (:local.get ed) (:local.get cd) (:local.get bi) :i64.mul :i64.add
              (:local.get cc) (:local.get ai) :i64.mul :i64.sub (:local.set ed)
              (:local.get c) (:local.get i) (:local.get ec) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64
              (:array.set :limbs)
              (:local.get d) (:local.get i) (:local.get ed) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64
              (:array.set :limbs)
              (:local.get ec) (:i64.const ,l) :i64.shr_s (:local.set ec)
              (:local.get ed) (:i64.const ,l) :i64.shr_s (:local.set ed)
              (:local.get i) (:i32.const 1) :i32.add (:local.set i)
              (:br aagain)))
          (:local.get la) (:local.set lb)
          ,@(lehmer-trim-code 'c 'la)
          ,@(lehmer-trim-code 'd 'lb)
          (:local.get a) (:local.get c) (:local.set a) (:local.set c)
          (:local.get b) (:local.get d) (:local.set b) (:local.set d)
          (:br outer)))
     ;; the results, of exact lengths
     '(:global.get :mv-buf) '(:i32.const 0)
     '(:local.get lb) '(:array.new_default :limbs) '(:local.tee tmp)
     '(:i32.const 0) '(:local.get b) '(:i32.const 0) '(:local.get lb) '(:array.copy :limbs :limbs)
     '(:local.get tmp) '(:i32.const 0) '(:call :make-int) '(:array.set :vector)
     '(:i32.const 2) '(:global.set :mv-count)
     '(:local.get la) '(:array.new_default :limbs) '(:local.tee tmp)
     '(:i32.const 0) '(:local.get a) '(:i32.const 0) '(:local.get la) '(:array.copy :limbs :limbs)
     '(:local.get tmp) '(:i32.const 0) '(:call :make-int))))
(defprim clysm-sys::%lehmer (a b) :mv `(,@a ,@b (:call :lehmer)))

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
;; the layout compared inline, then the included structures, the class
;; precedence list searched by the helper for other classes
(defprim clysm-sys::%structure-typep (x c) :test
  (let ((o (new-local)) (cl (new-local)))
    `(,@x (:local.set ,o) ,@c (:local.set ,cl) ,@(struct-check-code o cl))))
;; of a structure of N field slots: its instances (and those of the
;; structures including it) are a $si<N> or a subtype, tested first
(defprim clysm-sys::%structure-typep (x c n) :test
  (let* ((o (new-local)) (cl (new-local)) (size (struct-size-index n)) (l (new-local))
         (s (and size (new-local `(:ref :null ,(sinst-type size))))))
    (if (null size)
        `(,@x (:local.set ,o) ,@c (:local.set ,cl) ,@n :drop
          ,@(let ((d (struct-depth-index n)))
              (if d (struct-check-depth-code o cl d) (struct-check-code o cl))))
        (let ((ty (sinst-type size)))
          `(,@x (:local.set ,o) ,@c (:local.set ,cl)
            (:local.get ,o) (:ref.test ,ty)
            (:if nil :i32
                 ((:local.get ,o) (:ref.cast! ,ty) (:local.tee ,s) (:struct.get ,ty 0) (:local.tee ,l) (:local.get ,cl)
                  :ref.eq
                  (:if nil :i32 ((:i32.const 1))
                       ,(sinst-inherits-check-code s ty cl (struct-depth-index n)
                                                   `((:local.get ,o) (:local.get ,cl) (:call :struct-typep)))))
                 ;; a structure of fewer field slots is not one; an instance
                 ;; of a structure of a slot vector including it may be
                 ((:local.get ,o) (:ref.test :si1)
                  (:if nil :i32 ((:i32.const 0)) ,(struct-check-code o cl)))))))))

;; (when src/typed.lisp does not compile them: the interpreter's functions)
(defprim clysm-sys::%make-tstruct :any :value
  `(,@(apply #'append args) ,@(lisp-call-instrs 'clysm-sys::%make-tstruct (length args))))
(defprim clysm-sys::%make-tstruct-default (class kinds) :value
  `(,@class ,@kinds ,@(lisp-call-instrs 'clysm-sys::%make-tstruct-default 2)))
(defprim clysm-sys::%copy-tstruct (x class kinds) :value
  `(,@x ,@class ,@kinds ,@(lisp-call-instrs 'clysm-sys::%copy-tstruct-slow 3)))
;;; Structures with typed slots (runtime/struct.lisp): the code is in
;;; src/typed.lisp; these are what is left when it does not apply (the slot
;;; kinds are a constant there): the generic access by index.
(defprim clysm-sys::%struct-tref (x class i kinds) :value
  (let ((o (new-local)) (c (new-local)) (kk (new-local)))
    `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,kk) ,@kinds :drop
      ,@(struct-check-code o c)
      (:if nil :eqref
           ((:local.get ,o) (:local.get ,kk) (:call :instance-ref))
           ((:local.get ,o) (:local.get ,c) (:local.get ,kk) ,@(lisp-call-instrs 'clysm-sys::%struct-ref 3))))))
(defprim clysm-sys::%struct-tset (x class i v kinds) :value
  (let ((o (new-local)) (c (new-local)) (kk (new-local)) (val (new-local)))
    `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,kk) ,@v (:local.set ,val) ,@kinds :drop
      ,@(struct-check-code o c)
      (:if nil :eqref
           ((:local.get ,o) (:local.get ,kk) (:local.get ,val) (:call :instance-set))
           ((:local.get ,o) (:local.get ,c) (:local.get ,kk) (:local.get ,val)
            ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))))))
;; COPY-STRUCTURE: the layout and a copy of the slots
(defprim clysm-sys::%copy-instance (x) :value
  (let ((o (new-local '(:ref :null :instance))) (v (new-local '(:ref :null :vector))) (n (new-local :i32))
        (e (new-local)))
    `(,@x (:local.tee ,e) (:ref.test :sfield)
      (:if nil :eqref
           ((:local.get ,e) (:ref.cast! :instance) (:struct.get :instance 0) (:ref.cast :instance)
            (:struct.get :instance 1) (:i32.const ,+class-shape-ops-slot+) (:array.get :vector) ,@(nil-code) :ref.eq
            (:if nil :eqref ((:local.get ,e) (:call :sinst-copy))
                 ((:local.get ,e) ,@(lisp-call-instrs 'clysm-sys::%tstruct-copy 1))))
           ((:local.get ,e) (:ref.cast :instance) (:local.tee ,o) (:struct.get :instance 0)
            (:local.get ,o) (:struct.get :instance 1) (:local.tee ,v) :array.len (:local.tee ,n)
            (:array.new_default :vector) (:local.tee ,v)
            (:i32.const 0) (:local.get ,o) (:struct.get :instance 1) (:i32.const 0) (:local.get ,n)
            (:array.copy :vector :vector)
            (:local.get ,v) :ref.as_non_null (:i32.const 0) (:struct.new :instance))))))

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
                        (:i32.const ,+char-tag+) :i32.add :ref.i31 :return))
          ,@(unless strings-only
              `((:local.get ,d) :ref.is_null :i32.eqz
                (:if nil nil ((:local.get ,d) (:local.get ,i) (:call :typed-ref) (:local.tee ,d) :ref.is_null :i32.eqz
                              (:if nil nil ((:local.get ,d) :return))))))))))

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
                              (:local.get 2) :return))))
          ,@(unless strings-only
              `((:local.get ,d) :ref.is_null :i32.eqz
                (:if nil nil ((:local.get ,d) (:local.get ,i) (:local.get 2) (:call :typed-set)
                              (:if nil nil ((:local.get 2) :return))))))))))

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

;;; string hashing (29 bits, as STRING-HASH in the compiler) and comparison:
;;; FNV-1a over the code points; from 8 characters on, four FNV-1a hashes
;;; of every fourth character (independent chains of multiplications, about
;;; three times as fast on long strings) combined by FNV steps.
(defun string-hash-loop-code (str i n h h1 h2 h3)
  "Instructions hashing the characters I to N (locals, I is advanced) of
the $string in local STR into the i32 local H."
  (flet ((lane-step (lane k)
           `((:local.get ,lane) (:local.get ,str) (:local.get ,i)
             ,@(unless (eql k 0) `((:i32.const ,k) :i32.add))
             (:array.get :string) :i32.xor (:i32.const ,#x01000193) :i32.mul (:local.set ,lane)))
         (mix (lane)
           `((:local.get ,h) (:local.get ,lane) :i32.xor (:i32.const ,#x01000193) :i32.mul (:local.set ,h))))
    (let ((offset (- #x811C9DC5 (expt 2 32))))
      (flet ((one-by-one (done again)
               `((:block ,done nil
                   (:loop ,again nil
                     (:local.get ,i) (:local.get ,n) :i32.ge_u (:br_if ,done)
                     ,@(lane-step h 0)
                     (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
                     (:br ,again))))))
        `((:i32.const ,offset) (:local.set ,h)
          (:local.get ,n) (:local.get ,i) :i32.sub (:i32.const 8) :i32.ge_u
          (:if nil nil
               ((:i32.const ,(+ offset 1)) (:local.set ,h1)
                (:i32.const ,(+ offset 2)) (:local.set ,h2)
                (:i32.const ,(+ offset 3)) (:local.set ,h3)
                (:block four nil
                  (:loop again4 nil
                    (:local.get ,i) (:i32.const 4) :i32.add (:local.get ,n) :i32.gt_u (:br_if four)
                    ,@(lane-step h 0) ,@(lane-step h1 1) ,@(lane-step h2 2) ,@(lane-step h3 3)
                    (:local.get ,i) (:i32.const 4) :i32.add (:local.set ,i)
                    (:br again4)))
                ,@(one-by-one 'tail 'again-tail)
                ,@(mix h1) ,@(mix h2) ,@(mix h3))
               ,(one-by-one 'done 'again)))))))

(defhelper :string-hash :e-e ((str (:ref :null :string)) (i :i32) (n :i32) (h :i32) (h1 :i32) (h2 :i32) (h3 :i32))
  '(:local.get 0) '(:ref.test :string)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :string) (:local.tee str) :array.len (:local.set n)
         ,@(string-hash-loop-code 'str 'i 'n 'h 'h1 'h2 'h3)
         (:local.get h) (:i32.const ,#x1FFFFFFF) :i32.and :ref.i31 :return))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%string-hash)) '(:call (:callsym 1)))
(defprim clysm-sys::%string-hash (s) :value `(,@s (:call :string-hash)))
;; the same hash of the characters of the simple string STR from START to
;; END (a string with a fill pointer, displaced: its storage)
(defhelper :string-hash-range :eee-e ((str (:ref :null :string)) (i :i32) (n :i32) (h :i32) (h1 :i32) (h2 :i32) (h3 :i32))
  '(:local.get 0) '(:ref.cast :string) '(:local.set str)
  '(:local.get 1) '(:ref.cast :i31) ':i31.get_s '(:local.set i)
  '(:local.get 2) '(:ref.cast :i31) ':i31.get_s '(:local.set n)
  (string-hash-loop-code 'str 'i 'n 'h 'h1 'h2 'h3)
  '(:local.get h) '(:i32.const #x1FFFFFFF) ':i32.and ':ref.i31)
(defprim clysm-sys::%string-hash-range (s start end) :value `(,@s ,@start ,@end (:call :string-hash-range)))

;;; Hash tables (runtime/hash.lisp): the entry whose key is EQ to KEY, of
;;; hash HASH, in the table whose slot vector is SV; its index or -1.  The
;;; slot vector and its arrays are cast once, not at every access.  The
;;; slot indices are those of runtime/hash.lisp (+HT-KEYS+ ...).
(defhelper :ht-eq-index :eei-e ((v (:ref :null :vector)) (keys (:ref :null :vector)) (next (:ref :null :limbs))
                                (i :i32) (j :i32) (bits :i32) (y :i32))
  '(:local.get 0) '(:ref.cast :vector) '(:local.tee v)
  '(:i32.const 2) '(:array.get :vector) '(:ref.cast :vector) '(:local.set keys)
  '(:local.get v) '(:i32.const 4) '(:array.get :vector) '(:ref.cast :limbs) '(:local.set next)
  '(:local.get v) '(:i32.const 12) '(:array.get :vector) '(:ref.cast :i31) ':i31.get_s '(:local.set bits)
  ;; the chain: (hash + y*40503 + (y >> bits)*31153) & (2^bits - 1), y = hash >> bits
  '(:local.get 2) '(:local.get bits) ':i32.shr_s '(:local.set y)
  '(:local.get v) '(:i32.const 6) '(:array.get :vector) '(:ref.cast :limbs)
  '(:local.get 2) '(:local.get y) '(:i32.const 40503) ':i32.mul ':i32.add
  '(:local.get y) '(:local.get bits) ':i32.shr_s '(:i32.const 31153) ':i32.mul ':i32.add
  '(:i32.const 1) '(:local.get bits) ':i32.shl '(:i32.const 1) ':i32.sub ':i32.and
  '(:array.get :limbs) '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set j)
       (:local.get keys) (:local.get j) (:array.get :vector) (:local.get 1) :ref.eq
       (:if nil nil ((:local.get j) :ref.i31 :return))
       (:local.get next) (:local.get j) (:array.get :limbs) (:local.set i)
       (:br again)))
  '(:i32.const -1) ':ref.i31)
(defprim clysm-sys::%ht-eq-index (sv key hash) :value `(,@sv ,@key ,@(idx-code hash) (:call :ht-eq-index)))
;;; The index of the entry of KEY in the table whose slot vector is SV when
;;; KEY is a fixnum, a character or a symbol, which EQ finds (not a fixnum
;;; or a character in an EQUALP table): the index, -1 when there is none,
;;; -2 for another key.  The hash is that of %HT-HASH.
(defhelper :ht-fast-index :ee-e ((v (:ref :null :vector)) (hash :i32) (kind :i32))
  '(:local.get 0) '(:ref.cast :vector) '(:local.tee v)
  '(:i32.const 10) '(:array.get :vector) '(:ref.cast :i31) ':i31.get_s '(:local.set kind)
  '(:block found nil
     (:block notimm :eqref
       (:local.get 1) (:br_on_cast_fail notimm :eqref (:ref :i31)) :i31.get_s (:local.tee hash)
       ;; an EQUALP table compares numbers with = and characters ignoring case
       (:local.get kind) (:i32.const 3) :i32.eq (:if nil nil ((:i32.const -2) :ref.i31 :return))
       (:i32.const 536870912) :i32.ge_s
       (:if nil nil
            ;; a character: its code
            ((:local.get hash) (:i32.const 536870912) :i32.sub (:local.set hash) (:br found)))
       ;; a fixnum
       (:local.get hash) (:i32.const 536870911) :i32.and (:local.set hash) (:br found))
     :drop
     (:local.get 1) (:ref.test :symbol) :i32.eqz (:if nil nil ((:i32.const -2) :ref.i31 :return))
     (:local.get 1) (:ref.cast :symbol) (:struct.get :symbol 5) (:local.set hash))
  '(:local.get v) '(:local.get 1) '(:local.get hash) '(:call :ht-eq-index))
(defprim clysm-sys::%ht-fast-index (sv key) :value `(,@sv ,@key (:call :ht-fast-index)))
;;; GETHASH and (SETF GETHASH) of an existing entry at the call site, in one
;;; call each: H is any object.  When it is a hash table and KEY a fixnum, a
;;; character or a symbol that EQ finds (as :HT-FAST-INDEX), :HT-GET returns
;;; the value of the entry of KEY, or +DELETED+ (which is never a value)
;;; when there is none, and :HT-PUT-OLD stores VALUE in the entry of KEY and
;;; returns it; else (another key, another object, no entry to store in)
;;; they return null and the call site calls the function.  The test that H
;;; is a hash table is in the helper, so the call site is a call and a branch.
(defun ht-index-code (h k i)
  "Code leaving in the local I the index of the entry of the local K in the
local H (an eqref), -1 when there is none; null is returned for another key
or object."
  (progn
    `((:local.get ,h) (:ref.test :instance) :i32.eqz (:if nil nil ((:ref.null :eq) :return))
      (:local.get ,h) (:ref.cast :instance) (:struct.get :instance 0)
      (:global.get ,(sym-global 'clysm-sys::*hash-table-class*)) (:struct.get :symbol ,+sym-value+) :ref.eq
      :i32.eqz (:if nil nil ((:ref.null :eq) :return))
      (:local.get ,h) (:ref.cast :instance) (:struct.get :instance 1) (:local.get ,k) (:call :ht-fast-index)
      (:ref.cast :i31) :i31.get_s (:local.tee ,i) (:i32.const -2) :i32.eq (:if nil nil ((:ref.null :eq) :return)))))
(defhelper :ht-get :ee-e ((i :i32))
  (ht-index-code 0 1 'i)
  '(:local.get i) '(:i32.const 0) :i32.lt_s
  `(:if nil nil ((:global.get ,(sym-global 'clysm-sys::+deleted+)) (:struct.get :symbol ,+sym-value+) :return))
  '(:local.get 0) '(:ref.cast :instance) '(:struct.get :instance 1) '(:i32.const 3) '(:array.get :vector)
  '(:ref.cast :vector) '(:local.get i) '(:array.get :vector))
(defhelper :ht-put-old :eee-e ((i :i32))
  (ht-index-code 0 1 'i)
  '(:local.get i) '(:i32.const 0) :i32.lt_s '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get 0) '(:ref.cast :instance) '(:struct.get :instance 1) '(:i32.const 3) '(:array.get :vector)
  '(:ref.cast :vector) '(:local.get i) '(:local.get 2) '(:array.set :vector)
  '(:local.get 2))
;;; (%HT-GET H KEY SLOW): the value of KEY in H or +DELETED+, SLOW (code
;;; computing the same, a call) when the helper cannot say
(defprim clysm-sys::%ht-get (h key slow) :value
  (let ((done (gensym "HTGET")))
    `((:block ,done :eqref ,@h ,@key (:call :ht-get) (:br_on_non_null ,done) ,@slow))))
(defprim clysm-sys::%ht-absent () :value
  `((:global.get ,(sym-global 'clysm-sys::+deleted+)) (:struct.get :symbol ,+sym-value+)))
(defprim clysm-sys::%ht-put-old (h key value slow) :value
  (let ((done (gensym "HTPUT")))
    `((:block ,done :eqref ,@h ,@key ,@value (:call :ht-put-old) (:br_on_non_null ,done) ,@slow))))
;;; The index of the entry whose key is a string equal to the N characters
;;; of the simple string D from OFF (a simple string key: D itself; a string
;;; with a fill pointer or displaced: its storage), of hash HASH
;;; (%STRING-HASH), in the table whose slot vector is SV, or -1: the stored
;;; hashes are compared first, then the characters of keys that are simple
;;; strings or strings with a fill pointer over a simple string (JSON
;;; readers keep their buffers as keys); -2 when a key of the same hash is
;;; another object (left to EQUAL).
(defhelper :ht-region-index :eeiii-e ((v (:ref :null :vector)) (keys (:ref :null :vector)) (next (:ref :null :limbs))
                                       (hashes (:ref :null :limbs)) (i :i32) (j :i32) (bits :i32) (y :i32) (k :eqref)
                                       (d (:ref :null :string)) (ks (:ref :null :string)) (c :i32) (koff :i32)
                                       (m (:ref :null :mdarray)))
  '(:local.get 0) '(:ref.cast :vector) '(:local.tee v)
  '(:i32.const 2) '(:array.get :vector) '(:ref.cast :vector) '(:local.set keys)
  '(:local.get v) '(:i32.const 4) '(:array.get :vector) '(:ref.cast :limbs) '(:local.set next)
  '(:local.get v) '(:i32.const 5) '(:array.get :vector) '(:ref.cast :limbs) '(:local.set hashes)
  '(:local.get v) '(:i32.const 12) '(:array.get :vector) '(:ref.cast :i31) ':i31.get_s '(:local.set bits)
  '(:local.get 1) '(:ref.cast :string) '(:local.set d)
  '(:local.get 2) '(:local.get bits) ':i32.shr_s '(:local.set y)
  '(:local.get v) '(:i32.const 6) '(:array.get :vector) '(:ref.cast :limbs)
  '(:local.get 2) '(:local.get y) '(:i32.const 40503) ':i32.mul ':i32.add
  '(:local.get y) '(:local.get bits) ':i32.shr_s '(:i32.const 31153) ':i32.mul ':i32.add
  '(:i32.const 1) '(:local.get bits) ':i32.shl '(:i32.const 1) ':i32.sub ':i32.and
  '(:array.get :limbs) '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set j)
       (:local.get next) (:local.get j) (:array.get :limbs) (:local.set i)
       (:local.get hashes) (:local.get j) (:array.get :limbs) (:local.get 2) :i32.eq
       (:if nil nil
            ((:local.get keys) (:local.get j) (:array.get :vector) (:local.tee k)
             (:ref.test :string)
             (:if nil nil
                  ((:local.get k) (:ref.cast :string) (:local.set ks) (:i32.const 0) (:local.set koff)
                   (:local.get ks) :array.len (:local.get 4) :i32.ne (:br_if again))
                  ;; a string with a fill pointer whose storage is a simple string
                  ((:local.get k) (:ref.test :mdarray) :i32.eqz
                   (:if nil nil ((:i32.const -2) :ref.i31 :return))
                   (:local.get k) (:ref.cast :mdarray) (:local.tee m) (:struct.get :mdarray 0) (:ref.test :string)
                   (:local.get m) (:struct.get :mdarray 1) (:ref.test :i31) :i32.and :i32.eqz
                   (:if nil nil ((:i32.const -2) :ref.i31 :return))
                   (:local.get m) (:struct.get :mdarray 0) (:ref.cast :string) (:local.set ks)
                   (:local.get m) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s (:local.set koff)
                   (:local.get m) (:struct.get :mdarray 1) (:ref.cast :i31) :i31.get_s (:local.get 4) :i32.ne
                   (:br_if again)))
             (:i32.const 0) (:local.set c)
             (:block differ nil
               (:loop chars nil
                 (:local.get c) (:local.get 4) :i32.ge_u
                 (:if nil nil ((:local.get j) :ref.i31 :return))
                 (:local.get ks) (:local.get koff) (:local.get c) :i32.add (:array.get :string)
                 (:local.get d) (:local.get 3) (:local.get c) :i32.add (:array.get :string) :i32.ne
                 (:br_if differ)
                 (:local.get c) (:i32.const 1) :i32.add (:local.set c)
                 (:br chars)))))
       (:br again)))
  '(:i32.const -1) ':ref.i31)
;;; (a simple string key)
(defprim clysm-sys::%ht-string-index (sv key hash) :value
  (let ((k (new-local)))
    `(,@sv ,@key (:local.tee ,k) ,@(idx-code hash) (:i32.const 0)
      (:local.get ,k) (:ref.cast :string) :array.len (:call :ht-region-index))))
(defprim clysm-sys::%ht-region-index (sv d hash off n) :value
  `(,@sv ,@d ,@(idx-code hash) ,@(idx-code off) ,@(idx-code n) (:call :ht-region-index)))
(defprim clysm-sys::%immediate-or-symbol-p (x) :test
  ;; a fixnum, a character or a symbol: an i31 or a $symbol
  (let ((v (new-local))) `(,@x (:local.tee ,v) (:ref.test :i31) (:local.get ,v) (:ref.test :symbol) :i32.or)))

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
;;; The N characters of the simple string A from AI and of the simple string
;;; B from BI are the same (the caller checks the bounds): a string with a
;;; fill pointer or displaced compared over its storage
(defhelper :string-region= :eieii-i ((a (:ref :null :string)) (b (:ref :null :string)) (i :i32))
  '(:local.get 0) '(:ref.cast :string) '(:local.set a)
  '(:local.get 2) '(:ref.cast :string) '(:local.set b)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get 4) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get 1) (:local.get i) :i32.add (:array.get :string)
       (:local.get b) (:local.get 3) (:local.get i) :i32.add (:array.get :string) :i32.ne
       (:if nil nil ((:i32.const 0) :return))
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:i32.const 1))
(defprim clysm-sys::%string-region= (a ai b bi n) :test
  `(,@a ,@(idx-code ai) ,@b ,@(idx-code bi) ,@(idx-code n) (:call :string-region=)))

;;; ------------------------------------------------------------
;;; Rationals within 64 bits
;;; ------------------------------------------------------------
;;; A rational whose numerator and denominator are within 64 bits (an
;;; integer, or a ratio of two such) is computed in i64: common factors are
;;; divided out first (Knuth 4.5.1), every product and sum is checked, and
;;; the result is reduced by a gcd on i64.  Larger components and overflows
;;; go to the Lisp functions.
(defhelper :gcd64 :ll-l ((a :i64) (b :i64) (tt :i64) (sh :i64))
  ;; of two magnitudes (unsigned): the binary algorithm
  '(:local.get 0) '(:local.set a) '(:local.get 1) '(:local.set b)
  '(:local.get a) '(:i64.const 1) :i64.eq '(:local.get b) '(:i64.const 1) :i64.eq :i32.or
  '(:if nil nil ((:i64.const 1) :return))
  '(:local.get a) :i64.eqz '(:if nil nil ((:local.get b) :return))
  '(:local.get b) :i64.eqz '(:if nil nil ((:local.get a) :return))
  '(:local.get a) '(:local.get b) :i64.or :i64.ctz '(:local.set sh)
  '(:local.get a) '(:local.get a) :i64.ctz :i64.shr_u '(:local.set a)
  '(:loop again nil
     (:local.get b) (:local.get b) :i64.ctz :i64.shr_u (:local.set b)
     ;; a := the smaller, b := the difference (without a branch)
     (:local.get b) (:local.get a) (:local.get a) (:local.get b) :i64.gt_u :select (:local.set tt)
     (:local.get a) (:local.get b) :i64.sub (:local.get b) (:local.get a) :i64.sub
     (:local.get a) (:local.get b) :i64.gt_u :select (:local.set b)
     (:local.get tt) (:local.set a)
     (:local.get b) :i64.eqz :i32.eqz (:br_if again))
  '(:local.get a) '(:local.get sh) :i64.shl)

(defun small-int-field-code (value-code x fail)
  "The integer left by VALUE-CODE into the i64 local X when it is a fixnum or
within 64 bits (and not -2^63), else a branch to FAIL.  Uses the locals EE and V."
  `(,@value-code (:local.tee ee) (:ref.test :i31)
    (:if nil nil
         ((:local.get ee) (:ref.cast! :i31) :i31.get_s (:local.tee v)
          (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
          (:local.get v) :i64.extend_i32_s (:local.set ,x))
         ((:local.get ee) (:ref.test :bignum) :i32.eqz (:br_if ,fail)
          (:local.get ee) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee ,x) :i64.eqz (:br_if ,fail)
          (:local.get ,x) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if ,fail)))))

(defun rat-decode-code (p n d fail)
  "Parameter P as numerator N and denominator D (i64 locals), or a branch to FAIL."
  `((:local.get ,p) (:ref.test :ratio)
    (:if nil nil
         (,@(small-int-field-code `((:local.get ,p) (:ref.cast! :ratio) (:struct.get :ratio 0)) n fail)
          ,@(small-int-field-code `((:local.get ,p) (:ref.cast! :ratio) (:struct.get :ratio 1)) d fail))
         (,@(small-int-field-code `((:local.get ,p)) n fail)
          (:i64.const 1) (:local.set ,d)))))

(defun abs64-code (x) `((:local.get ,x) (:i64.const 0) :i64.lt_s (:if nil :i64 ((:i64.const 0) (:local.get ,x) :i64.sub) ((:local.get ,x)))))

(defun rat-mul-code (x y r fail) (i64-checked-op-code :mul x y r fail))

;;; N / D (D > 0) in lowest terms: an integer or a ratio
;;; A / G for a common factor G of A (often 1: no division)
(defhelper :div-common :ll-l ()
  '(:local.get 1) '(:i64.const 1) :i64.eq
  '(:if nil :i64 ((:local.get 0)) ((:local.get 0) (:local.get 1) :i64.div_s)))
(defhelper :make-ratio64 :ll-e ((n :i64) (d :i64) (g :i64))
  '(:local.get 0) :i64.eqz '(:if nil nil ((:i32.const 0) :ref.i31 :return))
  (abs64-code 0) '(:local.get 1) '(:call :gcd64) '(:local.set g)
  '(:local.get 0) '(:local.get g) '(:call :div-common) '(:local.set n)
  '(:local.get 1) '(:local.get g) '(:call :div-common) '(:local.tee d) '(:i64.const 1) :i64.eq
  '(:if nil nil ((:local.get n) (:call :i64-int) :return))
  '(:local.get n) '(:call :i64-int) '(:local.get d) '(:call :i64-int) '(:struct.new :ratio))

;;; N / D already in lowest terms (D > 0)
(defhelper :make-ratio-reduced :ll-e ()
  '(:local.get 1) '(:i64.const 1) :i64.eq
  '(:if nil nil ((:local.get 0) (:call :i64-int) :return))
  '(:local.get 0) :i64.eqz '(:if nil nil ((:i32.const 0) :ref.i31 :return))
  '(:local.get 0) '(:call :i64-int) '(:local.get 1) '(:call :i64-int) '(:struct.new :ratio))

;;; OP: 0 + 1 - 2 * 3 /.  Null when an operand is not such a rational, a
;;; step overflows or the divisor is zero.
(defhelper :ratio-op :eei-e ((v :i32) (ee :eqref) (na :i64) (da :i64) (nb :i64) (db :i64)
                             (g :i64) (x :i64) (y :i64) (n :i64) (d :i64))
  `(:block fail nil
     ,@(rat-decode-code 0 'na 'da 'fail)
     ,@(rat-decode-code 1 'nb 'db 'fail)
     (:local.get 2) (:i32.const 2) :i32.lt_u
     (:if nil nil
          (;; na/da +- nb/db with g = gcd(da, db): (na db/g +- nb da/g) / (da/g db)
           (:local.get da) (:local.get db) (:call :gcd64) (:local.set g)
           (:local.get db) (:local.get g) (:call :div-common) (:local.set y)
           ,@(rat-mul-code 'na 'y 'x 'fail)
           (:local.get da) (:local.get g) (:call :div-common) (:local.set da)
           ,@(rat-mul-code 'nb 'da 'y 'fail)
           (:local.get 2)
           (:if nil nil ,(i64-checked-op-code :sub 'x 'y 'n 'fail) ,(i64-checked-op-code :add 'x 'y 'n 'fail))
           ;; the sum has no factor in common with da/g db/g, only with g
           (:local.get g) (:i64.const 1) :i64.eq
           (:if nil nil
                (,@(rat-mul-code 'da 'db 'd 'fail)
                 (:local.get n) (:local.get d) (:call :make-ratio-reduced) :return))
           (:local.get n) :i64.eqz (:if nil nil ((:i32.const 0) :ref.i31 :return))
           ,@(abs64-code 'n) (:local.get g) (:call :gcd64) (:local.set g)
           (:local.get n) (:local.get g) (:call :div-common) (:local.set n)
           (:local.get db) (:local.get g) (:call :div-common) (:local.set db)
           ,@(rat-mul-code 'da 'db 'd 'fail)
           (:local.get n) (:local.get d) (:call :make-ratio-reduced) :return))
     (:local.get 2) (:i32.const 3) :i32.eq
     (:if nil nil
          (;; a division is a multiplication by the reciprocal db/nb
           (:local.get nb) :i64.eqz (:br_if fail)
           (:local.get nb) (:local.set x) (:local.get db) (:local.set nb) (:local.get x) (:local.set db)
           (:local.get db) (:i64.const 0) :i64.lt_s
           (:if nil nil ((:i64.const 0) (:local.get db) :i64.sub (:local.set db)
                         (:i64.const 0) (:local.get nb) :i64.sub (:local.set nb)))))
     ;; na/da * nb/db: gcd(na, db) and gcd(nb, da) divided out first
     ,@(abs64-code 'na) (:local.get db) (:call :gcd64) (:local.set g)
     (:local.get na) (:local.get g) (:call :div-common) (:local.set na) (:local.get db) (:local.get g) (:call :div-common) (:local.set db)
     ,@(abs64-code 'nb) (:local.get da) (:call :gcd64) (:local.set g)
     (:local.get nb) (:local.get g) (:call :div-common) (:local.set nb) (:local.get da) (:local.get g) (:call :div-common) (:local.set da)
     ,@(rat-mul-code 'na 'nb 'n 'fail)
     ,@(rat-mul-code 'da 'db 'd 'fail)
     (:local.get n) (:local.get d) (:call :make-ratio-reduced) :return)
  '(:ref.null :eq))

;;; -1, 0, 1 for A < B, A = B, A > B; 2 when an operand is not such a rational
(defhelper :ratio-cmp :ee-i ((v :i32) (ee :eqref) (na :i64) (da :i64) (nb :i64) (db :i64) (x :i64) (y :i64))
  `(:block fail nil
     ,@(rat-decode-code 0 'na 'da 'fail)
     ,@(rat-decode-code 1 'nb 'db 'fail)
     ,@(rat-mul-code 'na 'db 'x 'fail)
     ,@(rat-mul-code 'nb 'da 'y 'fail)
     (:i32.const -1) (:local.get x) (:local.get y) :i64.gt_s (:local.get x) (:local.get y) :i64.lt_s :select :return)
  '(:i32.const 2))

;;; TRUNCATE / FLOOR / CEILING / ROUND (MODE as for :divmod) of such
;;; rationals: the quotient, the remainder as the second value; or null
(defhelper :ratio-divmod :eei-e ((v :i32) (ee :eqref) (na :i64) (da :i64) (nb :i64) (db :i64) (g :i64)
                                 (pp :i64) (qq :i64) (dd :i64) (w :i64) (wr :i64) (wt :i64) (wt2 :i64))
  `(:block fail nil
     ,@(rat-decode-code 0 'na 'da 'fail)
     ,@(rat-decode-code 1 'nb 'db 'fail)
     (:local.get nb) :i64.eqz (:br_if fail)
     ;; with g = gcd(da, db): (na/da) / (nb/db) = P / Q for P = na db/g and
     ;; Q = da/g nb; the remainder is (P - q Q) / (da/g db)
     (:local.get da) (:local.get db) (:call :gcd64) (:local.set g)
     (:local.get db) (:local.get g) (:call :div-common) (:local.set wt)
     ,@(rat-mul-code 'na 'wt 'pp 'fail)
     (:local.get da) (:local.get g) (:call :div-common) (:local.set da)
     ,@(rat-mul-code 'da 'nb 'qq 'fail)
     ,@(rat-mul-code 'da 'db 'dd 'fail)
     (:local.get pp) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if fail)
     (:local.get qq) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if fail)
     (:local.get pp) (:local.get qq) :i64.div_s (:local.set w)
     (:local.get pp) (:local.get w) (:local.get qq) :i64.mul :i64.sub (:local.set wr)
     ,@(loop for mode from 1 to 3
             collect `(:local.get 2) collect `(:i32.const ,mode) collect :i32.eq
             collect `(:if nil nil ,(int-divmod-adjust-code :i64 mode 'pp 'qq 'w 'wr 'wt 'wt2)))
     (:global.get :mv-buf) (:i32.const 0)
     ;; (with an integer divisor the remainder is in lowest terms as the number was)
     (:local.get db) (:i64.const 1) :i64.eq
     (:if nil :eqref
          ((:local.get wr) (:local.get dd) (:call :make-ratio-reduced))
          ((:local.get wr) (:local.get dd) (:call :make-ratio64)))
     (:array.set :vector)
     (:i32.const 2) (:global.set :mv-count)
     (:local.get w) (:call :i64-int) :return)
  '(:ref.null :eq))

;;; RATIONAL of a float: an integer or m / 2^k within 62 bits; larger ones in Lisp
(defhelper :rational :e-e ((bits :i64) (m :i64) (ex :i32) (e2 :i32) (tz :i32) (neg :i32))
  `(:block slow nil
     (:local.get 0) (:ref.test :double)
     (:if nil nil
          ((:local.get 0) (:ref.cast! :double) (:struct.get :double 0) :i64.reinterpret_f64 (:local.set bits))
          ((:local.get 0) (:ref.test :single)
           (:if nil nil
                ((:local.get 0) (:ref.cast! :single) (:struct.get :single 0) :f64.promote_f32 :i64.reinterpret_f64
                 (:local.set bits))
                (;; a rational is returned as it is
                 (:local.get 0) (:call :integerp) (:local.get 0) (:ref.test :ratio) :i32.or :i32.eqz (:br_if slow)
                 (:local.get 0) :return))))
     (:local.get bits) (:i64.const 63) :i64.shr_u :i32.wrap_i64 (:local.set neg)
     (:local.get bits) (:i64.const 52) :i64.shr_u :i32.wrap_i64 (:i32.const #x7FF) :i32.and (:local.tee ex)
     (:i32.const #x7FF) :i32.eq (:br_if slow)
     (:local.get bits) (:i64.const #xFFFFFFFFFFFFF) :i64.and (:local.set m)
     (:local.get ex) :i32.eqz
     (:if nil nil
          ((:local.get m) :i64.eqz (:if nil nil ((:i32.const 0) :ref.i31 :return))
           (:i32.const -1074) (:local.set e2))
          ((:local.get m) (:i64.const #x10000000000000) :i64.or (:local.set m)
           (:local.get ex) (:i32.const 1075) :i32.sub (:local.set e2)))
     ;; an odd mantissa
     (:local.get m) :i64.ctz :i32.wrap_i64 (:local.set tz)
     (:local.get m) (:local.get tz) :i64.extend_i32_u :i64.shr_u (:local.set m)
     (:local.get e2) (:local.get tz) :i32.add (:local.set e2)
     (:local.get neg) (:if nil nil ((:i64.const 0) (:local.get m) :i64.sub (:local.set m)))
     (:local.get e2) (:i32.const 0) :i32.ge_s
     (:if nil nil
          (;; an integer: within 62 bits here
           (:i32.const 64) (:local.get m) (:local.get m) (:i64.const 63) :i64.shr_s :i64.xor :i64.clz :i32.wrap_i64 :i32.sub
           (:local.get e2) :i32.add (:i32.const 62) :i32.gt_s (:br_if slow)
           (:local.get m) (:local.get e2) :i64.extend_i32_u :i64.shl (:call :i64-int) :return))
     (:i32.const 0) (:local.get e2) :i32.sub (:local.tee e2) (:i32.const 62) :i32.gt_s (:br_if slow)
     (:local.get m) (:call :i64-int) (:i64.const 1) (:local.get e2) :i64.extend_i32_u :i64.shl (:call :i64-int)
     (:struct.new :ratio) :return)
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%rational-slow)) '(:call (:callsym 1)))
(defprim rational (x) :value `(,@x (:call :rational)))

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
  ;; digits: 2 (a magnitude of 2^63 or more), 3 or 4
  '(:i32.const 3) '(:i32.const 4) '(:local.get hi) '(:i64.const 32) :i64.shr_u :i64.eqz :select
  '(:i32.const 2) '(:local.get hi) :i64.eqz :i32.eqz :select
  '(:local.tee n) '(:array.new_default :limbs) '(:local.set m)
  '(:local.get m) '(:i32.const 0) '(:local.get lo) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get m) '(:i32.const 1) '(:local.get lo) '(:i64.const 32) :i64.shr_u :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get n) '(:i32.const 2) :i32.gt_u
  '(:if nil nil ((:local.get m) (:i32.const 2) (:local.get hi) :i32.wrap_i64 (:array.set :limbs)))
  '(:local.get n) '(:i32.const 3) :i32.gt_u
  '(:if nil nil ((:local.get m) (:i32.const 3) (:local.get hi) (:i64.const 32) :i64.shr_u :i32.wrap_i64
                 (:array.set :limbs)))
  '(:local.get 0) '(:local.get 1) :i64.xor '(:i64.const 0) :i64.lt_s
  '(:local.get m) '(:i64.const 0) '(:struct.new :bignum))

(defun arith-helper-body (opcode lisp-fn)
  "OPCODE: 0 + 1 - 2 * 3 /."
  (let ((min64 (- (expt 2 63)))
        (fop (ecase opcode (0 :f64.add) (1 :f64.sub) (2 :f64.mul) (3 :f64.div))))
    (flet ((finite-check (r sub zero ne)
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
        ;; complex numbers of floats: the Lisp functions for complex numbers
        ,@(complex-arith-code (ecase opcode (0 'clysm-sys::%complex-add) (1 'clysm-sys::%complex-sub)
                                (2 'clysm-sys::%complex-mul) (3 'clysm-sys::%complex-div)))
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
                (:local.get dr) (:struct.new :double) :return))
          (:local.get dr) :f32.demote_f64 (:local.set fr)
          ,@(finite-check 'fr :f32.sub '(:f32.const 0) :f32.ne)
          (:local.get fr) (:struct.new :single) :return)
        ;; integers beyond 64 bits (or a result beyond): the bignum helpers
        ,@(when (< opcode 3)
            `((:local.get 0) (:call :integerp)
              (:if nil nil
                   ((:local.get 1) (:call :integerp)
                    (:if nil nil
                         ((:local.get 0) (:local.get 1)
                          ,@(ecase opcode
                              (0 '((:i32.const 0) (:call :big-add)))
                              (1 '((:i32.const 1) (:call :big-add)))
                              (2 '((:call :big-mul))))
                          :return))))))
        ;; rationals with small components
        (:local.get 0) (:local.get 1) (:i32.const ,opcode) (:call :ratio-op) (:local.tee e) :ref.is_null :i32.eqz
        (:if nil nil ((:local.get e) :return))
        (:local.get 0) (:local.get 1)
        (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))))))

;;; The helpers the compiled code calls are small (the engine inlines them):
;;; two fixnums, or a double with a double or a fixnum; everything else is
;;; the general helper behind (:add-gen ...).
(defun f64-of-param-code (p v fail)
  "Push param P (a double, a single or a fixnum) as an f64, or branch to FAIL."
  `((:local.get ,p) (:ref.test :double)
    (:if nil :f64 ((:local.get ,p) (:ref.cast! :double) (:struct.get :double 0))
         ((:local.get ,p) (:ref.test :single)
          (:if nil :f64 ((:local.get ,p) (:ref.cast! :single) (:struct.get :single 0) :f64.promote_f32)
               ((:local.get ,p) (:ref.test :i31) :i32.eqz (:br_if ,fail)
                (:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee ,v)
                (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
                (:local.get ,v) :f64.convert_i32_s))))))

(defun f32-of-param-code (p v fail)
  "Push param P (a single or a fixnum) as an f32, or branch to FAIL."
  `((:local.get ,p) (:ref.test :single)
    (:if nil :f32 ((:local.get ,p) (:ref.cast! :single) (:struct.get :single 0))
         ((:local.get ,p) (:ref.test :i31) :i32.eqz (:br_if ,fail)
          (:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee ,v)
          (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
          (:local.get ,v) :f32.convert_i32_s))))

(defun single-arith-code (op zero-check)
  "When one argument is a single and the other a single or a fixnum: the
operation in f32 (locals SA, SB, SR), the generic function when the result is
not finite (or a zero that may be an underflow)."
  `((:local.get 0) (:ref.test :single) (:local.get 1) (:ref.test :single) :i32.or
    (:if nil nil
         ((:block slow nil
            ,@(f32-of-param-code 0 'v 'slow) (:local.set sa)
            ,@(f32-of-param-code 1 'v 'slow) (:local.set sb)
            (:local.get sa) (:local.get sb) ,op (:local.tee sr)
            (:local.get sr) :f32.sub (:f32.const 0.0) :f32.ne
            ,@(progn zero-check nil)
            (:br_if slow)
            (:local.get sr) (:struct.new :single) :return)))))

(defun complex-arith-code (lisp-fn)
  "When an argument is a complex of floats: the Lisp function for complex numbers."
  `((:local.get 0) (:ref.test :complex-d) (:local.get 0) (:ref.test :complex-s) :i32.or
    (:local.get 1) (:ref.test :complex-d) :i32.or (:local.get 1) (:ref.test :complex-s) :i32.or
    (:if nil nil ((:local.get 0) (:local.get 1) ,@(lisp-call-instrs lisp-fn 2) :return))))

(defun double-fast-path-code (body &optional singles)
  "When one argument is a double and the other a double, a single or a
fixnum (with SINGLES: also when one is a single and the other a single or a
fixnum, for comparisons, which are exact in f64): BODY with the arguments as
f64s in locals DA and DB (it may branch to SLOW)."
  `((:local.get 0) (:ref.test :double) (:local.get 1) (:ref.test :double) :i32.or
    ,@(when singles '((:local.get 0) (:ref.test :single) :i32.or (:local.get 1) (:ref.test :single) :i32.or))
    (:if nil nil
         ((:block slow nil
            ,@(f64-of-param-code 0 'v 'slow) (:local.set da)
            ,@(f64-of-param-code 1 'v 'slow) (:local.set db)
            ,@body)))))

(defun arith-front-body (opcode gen)
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
                    (:if nil nil ((:local.get x) (:local.get y) :i32.div_s (:call :box-i32) :return))))))))
    ,@(double-fast-path-code
       `(,@(when (= opcode 3) '((:local.get db) (:f64.const 0) :f64.eq (:br_if slow)))
         (:local.get da) (:local.get db) ,(ecase opcode (0 :f64.add) (1 :f64.sub) (2 :f64.mul) (3 :f64.div))
         (:local.set dr)
         ;; not finite: the general helper decides (an underflow is no condition)
         ,@(f64-not-finite-code (ecase opcode (0 '+) (1 '-) (2 '*) (3 '/)) 'dr)
         (:br_if slow)
         (:local.get dr) (:struct.new :double) :return))
    (:local.get 0) (:local.get 1) (:return_call ,gen)))

(macrolet ((arith (name gen opcode lisp-fn)
             `(progn
                (defhelper ,gen :ee-e ((x :i32) (y :i32) (r :i64) (v :i32) (xa :i64) (xb :i64)
                                       (ka :i32) (kb :i32) (da :f64) (db :f64) (dr :f64) (fr :f32) (e :eqref))
                  (cons :progn-marker (arith-helper-body ,opcode ',lisp-fn)))
                (defhelper ,name :ee-e ((x :i32) (y :i32) (r :i64) (v :i32) (da :f64) (db :f64) (dr :f64))
                  (cons :progn-marker (arith-front-body ,opcode ,gen))))))
  (arith :add :add-gen 0 clysm-sys::%generic-add)
  (arith :sub :sub-gen 1 clysm-sys::%generic-sub)
  (arith :mul :mul-gen 2 clysm-sys::%generic-mul)
  (arith :div :div-gen 3 clysm-sys::%generic-div))

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
      ;; rationals with small components
      (:local.get 0) (:local.get 1) (:call :ratio-cmp) (:local.tee x) (:i32.const 2) :i32.ne
      (:if nil nil ((:local.get x) ,@(if (eq iop :i64.lt_s) '((:i32.const 0) :i32.lt_s) '(:i32.eqz)) :return))
      (:local.get 0) (:local.get 1)
      (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))
      ,(car (nil-code)) :ref.eq :i32.eqz)))

(defun compare-front-body (iop fop gen)
  `(,@(fixnum-pair-code `((:local.get x) (:local.get y) ,iop :return))
    ,@(double-fast-path-code `((:local.get da) (:local.get db) ,fop :return))
    (:local.get 0) (:local.get 1) (:return_call ,gen)))

(defhelper :lt-gen :ee-i ((x :i32) (y :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-helper-body :i64.lt_s :f64.lt 'clysm-sys::%generic-lt)))
(defhelper :lt :ee-i ((x :i32) (y :i32) (v :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-front-body :i32.lt_s :f64.lt :lt-gen)))

(defhelper :num-eq-gen :ee-i ((x :i32) (y :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-helper-body :i64.eq :f64.eq 'clysm-sys::%generic-num-eq)))
(defhelper :num-eq :ee-i ((x :i32) (y :i32) (v :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-front-body :i32.eq :f64.eq :num-eq-gen)))

;;; dynamic binding
(defhelper :grow-bstack :v-v ((new (:ref :vector)))
  '(:ref.null :eq) '(:global.get :dyn) '(:struct.get :dynstate 1) :array.len '(:i32.const 2) :i32.mul
  '(:array.new :vector) '(:local.set new)
  '(:local.get new) '(:i32.const 0) '(:global.get :dyn) '(:struct.get :dynstate 1) '(:i32.const 0)
  '(:global.get :dyn) '(:struct.get :dynstate 0)
  '(:array.copy :vector :vector)
  '(:global.get :dyn) '(:local.get new) '(:struct.set :dynstate 1))

(defhelper :bind :ee-v ((sp :i32) (st (:ref :null :vector)))
  ;; grow if needed
  '(:global.get :dyn) '(:struct.get :dynstate 0) '(:local.tee sp) '(:i32.const 2) :i32.add
  '(:global.get :dyn) '(:struct.get :dynstate 1) :array.len :i32.gt_u
  '(:if nil nil ((:call :grow-bstack)))
  '(:global.get :dyn) '(:struct.get :dynstate 1) '(:local.set st)
  '(:local.get st) '(:local.get sp) '(:local.get 0) '(:array.set :vector)
  '(:local.get st) '(:local.get sp) '(:i32.const 1) :i32.add
  '(:local.get 0) '(:ref.cast! :symbol) `(:struct.get :symbol ,+sym-value+) '(:array.set :vector)
  '(:local.get 0) '(:ref.cast! :symbol) '(:local.get 1) `(:struct.set :symbol ,+sym-value+)
  '(:global.get :dyn) '(:local.get sp) '(:i32.const 2) :i32.add '(:struct.set :dynstate 0))

(defhelper :unbind-to :i-v ((sp :i32) (st (:ref :null :vector)))
  '(:global.get :dyn) '(:struct.get :dynstate 1) '(:local.set st)
  '(:global.get :dyn) '(:struct.get :dynstate 0) '(:local.set sp)
  '(:block done nil
     (:loop again nil
       (:local.get sp) (:local.get 0) :i32.le_s (:br_if done)
       (:local.get sp) (:i32.const 2) :i32.sub (:local.set sp)
       ;; sym.value = old
       (:local.get st) (:local.get sp) (:array.get :vector) (:ref.cast! :symbol)
       (:local.get st) (:local.get sp) (:i32.const 1) :i32.add (:array.get :vector)
       (:struct.set :symbol 2)
       ;; (the popped entries are not cleared: the next bindings overwrite them)
       (:br again)))
  '(:global.get :dyn) '(:local.get sp) '(:struct.set :dynstate 0))

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
  '(:global.get :dyn) '(:struct.get :dynstate 0) '(:local.set sp)
  `(:block done nil
     (:loop again nil
       (:local.get sp) (:i32.const 2) :i32.lt_s (:br_if done)
       (:local.get sp) (:i32.const 2) :i32.sub (:local.set sp)
       (:global.get :dyn) (:struct.get :dynstate 1) (:local.get sp) (:array.get :vector)
       (:global.get ,(sym-global 'clysm-sys::*catch-tags*)) :ref.eq
       (:if nil nil
            ((:global.get :dyn) (:struct.get :dynstate 1) (:local.get sp) (:i32.const 1) :i32.add (:array.get :vector)
             (:local.get 0) :ref.eq (:if nil nil (:return))))
       (:br again)))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%no-catch-error)) '(:call (:callsym 1)) :drop)

;;; is an exit pending (codegen.lisp, "Non-local exits as returns")?
(defhelper :exit-pending-p :v-i ()
  `(:global.get ,(sym-global 'clysm-sys::%exit-tag)) `(:struct.get :symbol ,+sym-value+) :ref.is_null :i32.eqz)

;;; the pending non-local exit (codegen.lisp, "Non-local exits as returns")
;;; is thrown
(defhelper :throw-pending-exit :v-v ((tag :eqref) (val :eqref))
  `(:global.get ,(sym-global 'clysm-sys::%exit-tag)) `(:struct.get :symbol ,+sym-value+) '(:local.set tag)
  `(:global.get ,(sym-global 'clysm-sys::%exit-value)) `(:struct.get :symbol ,+sym-value+) '(:local.set val)
  `(:global.get ,(sym-global 'clysm-sys::%exit-tag)) '(:ref.null :eq) `(:struct.set :symbol ,+sym-value+)
  `(:global.get ,(sym-global 'clysm-sys::%exit-value)) '(:ref.null :eq) `(:struct.set :symbol ,+sym-value+)
  '(:local.get tag) '(:local.get val) '(:throw :lisp-throw))

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

(defhelper :apply-list :ee-e ((f (:ref :null :closure)))
  ;; (apply f list): the entry of F taking a list
  '(:local.get 0) '(:call :coerce-function) '(:local.tee f) :ref.as_non_null
  '(:local.get 1) '(:local.get f) `(:struct.get :closure ,+clo-code+) `(:struct.get :code ,+clo-entry-a+)
  '(:return_call_ref :fA))

;; the entry taking a list of the arguments of the functions without one of
;; their own: up to four arguments go to the fixed entries without an
;; argument vector
(defhelper :apply-generic :fA ((l :eqref) (a0 :eqref) (a1 :eqref) (a2 :eqref) (a3 :eqref))
  (flet ((next (var k)
           ;; the list in L is a cons: its car to VAR; if the rest is not a
           ;; cons, call the entry for K + 1 arguments
           `((:local.get l) (:ref.cast! :cons) (:struct.get :cons 0) (:local.set ,var)
             (:local.get l) (:ref.cast! :cons) (:struct.get :cons 1) (:local.tee l)
             (:ref.test :cons) :i32.eqz
             (:if nil nil ((:local.get 0)
                           ,@(loop for v in '(a0 a1 a2 a3) repeat (1+ k) collect `(:local.get ,v))
                           (:local.get 0) ,@(clo-field-code (1+ k))
                           (:return_call_ref ,(fixed-entry-type (1+ k))))))))
    `((:local.get 1) (:local.tee l) (:ref.test :cons) :i32.eqz
      (:if nil nil ((:local.get 0) (:local.get 0) ,@(clo-field-code 0) (:return_call_ref :f0)))
      ,@(next 'a0 0) ,@(next 'a1 1) ,@(next 'a2 2) ,@(next 'a3 3)
      ;; more than four
      (:local.get 0) (:local.get 1) (:call :list-to-vector)
      (:local.get 0) (:struct.get :closure ,+clo-code+) (:struct.get :code ,+clo-entry-n+)
      (:return_call_ref :fN))))

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

(defun remove-helpers-of-kinds (kinds)
  "*HELPERS* without the helpers named (KIND k) for KIND in KINDS (one pass:
modules made at run time define these helpers each time)."
  (remove-if (lambda (h) (let ((name (first h))) (and (consp name) (member (car name) kinds))))
             *helpers*))

(defun define-argerr-helpers ()
  (setf *helpers* (remove-helpers-of-kinds '(:argerr :pack)))
  (loop for k from 0 to +max-fixed-args+
        do (let ((k k))
             (push (list (list :argerr k) (fixed-entry-type k) nil (lambda () (argerr-body k))) *helpers*)
             ;; pack entries for complex lambdas: build vector and call entryN
             (push (list (list :pack k) (fixed-entry-type k) nil
                         (lambda ()
                           `((:local.get 0)
                             ,@(loop for i from 1 to k collect `(:local.get ,i))
                             (:array.new_fixed :vector ,k)
                             (:local.get 0) ,@(clo-field-code +clo-entry-n+)
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
  ;; other objects: the host's hash, the last one kept (a GETHASH then a
  ;; PUTHASH of the same key ask once)
  '(:local.get 0) '(:global.get :ih-key) :ref.eq
  '(:if nil nil ((:global.get :ih-hash) :return))
  '(:local.get 0) :extern.convert_any '(:call :host-identity-hash) '(:local.set h)
  '(:local.get 0) '(:global.set :ih-key) '(:local.get h) '(:global.set :ih-hash)
  '(:local.get h))

(defhelper :to-f64 :e-d ((v :i32) (w :i64) (ee :eqref) (dn :f64) (dd :f64))
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
  ;; a ratio of integers that convert exactly: one correctly rounded division
  `(:block slow nil
     (:local.get 0) (:ref.test :ratio) :i32.eqz (:br_if slow)
     ,@(loop for field in '(0 1)
             append `((:block ok nil
                        (:local.get 0) (:ref.cast! :ratio) (:struct.get :ratio ,field) (:local.tee ee) (:ref.test :i31)
                        (:if nil nil ((:local.get ee) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s (:local.set w) (:br ok)))
                        (:local.get ee) (:ref.test :bignum) :i32.eqz (:br_if slow)
                        (:local.get ee) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee w) :i64.eqz (:br_if slow)
                        (:local.get w) (:i64.const ,(expt 2 53)) :i64.add (:i64.const ,(expt 2 54)) :i64.gt_u (:br_if slow))
                      (:local.get w) :f64.convert_i64_s (:local.set ,(if (= field 0) 'dn 'dd))))
     (:local.get dn) (:local.get dd) :f64.div :return)
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
;;; (with integers up to 2^53) itself, integers by truncating and
;;; adjusting by the remainder, floats as SBCL does (the float quotient
;;; rounded for the mode, then the remainder); everything else, and whatever
;;; would signal, goes to %DIVMOD-SLOW, which computes the same.

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

(defun float-divmod-code (mode slow da db q r adj single tmp &optional unit no-rem fq)
  "From the f64 locals DA and DB: the quotient in Q (integral, a float of the
sign of the quotient: -0.0 for a negative one rounded to zero) and the
remainder in R, as SBCL computes them: the float quotient rounded for MODE
\(truncated, down, up or to the nearest even integer), and number - quotient *
divisor (for an integer quotient, that integer times the divisor: 0.0 times
it for a quotient -0.0; FQ, the quotient wanted as a float: -0.0 times it).
ADJ (an i32 local)
is set to 0.  SINGLE (an i32 local, or NIL when the result is a double):
when set, the operands and every step are rounded to single precision (the
arithmetic of singles: an integer operand is first converted to a single).
Branches to SLOW when the divisor is zero or a value is not finite.  UNIT:
DB is known to be 1 (the division and the product are the identity).
NO-REM: the remainder is not wanted (not computed: its overflow does not
signal, as in SBCL where it is dead code); the quotient is tested instead."
  (flet ((rnd (l) (when single
                    `((:local.get ,single)
                      (:if nil nil ((:local.get ,l) :f32.demote_f64 :f64.promote_f32 (:local.set ,l))))))
         (op () (ecase mode (0 :f64.trunc) (1 :f64.floor) (2 :f64.ceil) (3 :f64.nearest))))
    `(,@(rnd da) ,@(unless unit (rnd db))
      ,@(if unit
            `((:local.get ,da) ,(op) (:local.set ,q))
            `((:local.get ,da) (:local.get ,db) :f64.div (:local.set ,q) ,@(rnd q)
              (:local.get ,q) ,(op) (:local.set ,q)))
      ,@(if no-rem
            `((:local.get ,q) (:local.get ,q) :f64.sub (:f64.const 0) :f64.ne (:hint 0) (:br_if ,slow))
            ;; remainder: number - quotient * divisor (not finite when the
            ;; quotient is not: a zero divisor, an infinity, a NaN)
            `((:local.get ,q) ,@(unless fq '((:f64.const 0) :f64.add)) (:local.set ,tmp)
              ,@(unless unit `((:local.get ,tmp) (:local.get ,db) :f64.mul (:local.set ,tmp) ,@(rnd tmp)))
              (:local.get ,da) (:local.get ,tmp) :f64.sub (:local.set ,r) ,@(rnd r)
              (:local.get ,r) (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne (:hint 0) (:br_if ,slow)))
      (:i32.const 0) (:local.set ,adj))))

(defhelper :divmod :eei-e ((x :i32) (y :i32) (qi :i32) (ri :i32) (ti :i32) (ti2 :i32)
                           (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64)
                           (w :i64) (wr :i64) (wt :i64) (wt2 :i64)
                           (q :f64) (r :f64) (tmp :f64) (adj :i32) (single :i32) (e :eqref))
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
  '(:local.get 0) '(:ref.test :ratio) '(:local.get 1) '(:ref.test :ratio) :i32.or
  '(:if nil nil
        ((:local.get 0) (:local.get 1) (:local.get 2) (:call :ratio-divmod) (:local.tee e) :ref.is_null :i32.eqz
         (:if nil nil ((:local.get e) :return)))
        ((:local.get 0) (:local.get 1) (:local.get 2) (:call :wide-divmod) (:local.tee e) :ref.is_null :i32.eqz
         (:if nil nil ((:local.get e) :return))))
  '(:local.get 0) '(:local.get 1) '(:local.get 2) :ref.i31
  `(:global.get ,(sym-global 'clysm-sys::%divmod-slow)) '(:call (:callsym 3)))

(defun single-divmod-code (mode)
  "For the front of a division: when one argument is a single and the other a
single or a fixnum, the division in f32 (what the general helper computes
with each step rounded to single: the f32 operations are correctly rounded),
returning; a value that is not finite or a quotient of 2^51 or more falls
through."
  (flet ((operand (i f)
           `((:local.get ,i) (:ref.test :single)
             (:if nil nil
                  ((:local.get ,i) (:ref.cast! :single) (:struct.get :single 0) (:local.set ,f))
                  ((:local.get ,i) (:ref.test :i31) :i32.eqz (:br_if nosingle)
                   (:local.get ,i) (:ref.cast! :i31) :i31.get_s (:local.tee x)
                   (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if nosingle)
                   (:local.get x) :f32.convert_i32_s (:local.set ,f))))))
    `((:block nosingle nil
        (:local.get 0) (:ref.test :single) (:local.get 1) (:ref.test :single) :i32.or :i32.eqz (:br_if nosingle)
        ,@(operand 0 'fa) ,@(operand 1 'fb)
        (:local.get fa) (:local.get fb) :f32.div ,(ecase mode (0 :f32.trunc) (1 :f32.floor) (2 :f32.ceil) (3 :f32.nearest))
        (:local.tee fq) :f32.abs (:f32.const 2251799813685248.0) :f32.lt :i32.eqz (:br_if nosingle)
        ;; remainder: the number - the integer quotient (0.0 for -0.0) times the divisor
        (:local.get fa) (:local.get fq) (:f32.const 0.0) :f32.add (:local.get fb) :f32.mul :f32.sub (:local.tee fr)
        (:local.get fr) :f32.sub (:f32.const 0.0) :f32.ne (:br_if nosingle)
        (:global.get :mv-buf) (:i32.const 0) (:local.get fr) (:struct.new :single) (:array.set :vector)
        (:i32.const 2) (:global.set :mv-count)
        (:local.get fq) :f64.promote_f32 (:local.set fd)
        (:local.get fd) (:f64.const 6755399441055744d0) :f64.add :i64.reinterpret_f64
        (:i64.const #x4338000000000000) :i64.sub (:call :i64-int) :return))))

;;; the front of each division: two fixnums inline (small enough for the
;;; engine to copy into the caller), one single and a single or a fixnum in
;;; f32, the general helper otherwise
(macrolet ((front (name mode)
             `(defhelper ,name :ee-e ((x :i32) (y :i32) (qi :i32) (ri :i32) (ti :i32) (ti2 :i32)
                                      (fa :f32) (fb :f32) (fq :f32) (fr :f32) (fd :f64))
                (fixnum-pair-code
                 `((:local.get y) :i32.eqz :i32.eqz
                   (:if nil nil
                        ((:local.get x) (:local.get y) :i32.div_s (:local.set qi)
                         (:local.get x) (:local.get y) :i32.rem_s (:local.set ri)
                         ,@(int-divmod-adjust-code :i32 ,mode 'x 'y 'qi 'ri 'ti 'ti2)
                         (:global.get :mv-buf) (:i32.const 0) (:local.get ri) :ref.i31 (:array.set :vector)
                         (:i32.const 2) (:global.set :mv-count)
                         (:local.get qi) (:call :box-i32) :return))))
                (single-divmod-code ,mode)
                '(:local.get 0) '(:local.get 1) '(:i32.const ,mode) '(:return_call :divmod))))
  (front :divmod0 0) (front :divmod1 1) (front :divmod2 2) (front :divmod3 3))

(macrolet ((div (name mode)
             `(progn
                (defprim ,name (a b) :mv `(,@a ,@b (:call ,,(intern (format nil "DIVMOD~D" mode) :keyword))))
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
;; the same for simple vectors and limbs (a hash table's arrays copied as they grow)
(defprim clysm-sys::%sv-copy (dst dstart src sstart n) :value
  (let ((d (new-local)))
    `(,@dst (:local.tee ,d) (:ref.cast :vector) ,@(idx-code dstart) ,@src (:ref.cast :vector) ,@(idx-code sstart)
      ,@(idx-code n) (:array.copy :vector :vector) (:local.get ,d))))
(defprim clysm-sys::%limbs-copy (dst dstart src sstart n) :value
  (let ((d (new-local)))
    `(,@dst (:local.tee ,d) (:ref.cast :limbs) ,@(idx-code dstart) ,@src (:ref.cast :limbs) ,@(idx-code sstart)
      ,@(idx-code n) (:array.copy :limbs :limbs) (:local.get ,d))))
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
;;; Complex numbers: a $complex-d holds two f64s, a $complex-s two f32s, a
;;; $complex the parts of any other complex number.
(defhelper :complexp :e-i ()
  '(:local.get 0) '(:ref.test :complex-d) '(:if nil nil ((:i32.const 1) :return))
  '(:local.get 0) '(:ref.test :complex-s) '(:if nil nil ((:i32.const 1) :return))
  '(:local.get 0) '(:ref.test :complex))
(defhelper :make-complex :ee-e ()
  '(:local.get 0) '(:ref.test :double) '(:local.get 1) '(:ref.test :double) :i32.and
  '(:if nil nil ((:local.get 0) (:ref.cast! :double) (:struct.get :double 0)
                 (:local.get 1) (:ref.cast! :double) (:struct.get :double 0) (:struct.new :complex-d) :return))
  '(:local.get 0) '(:ref.test :single) '(:local.get 1) '(:ref.test :single) :i32.and
  '(:if nil nil ((:local.get 0) (:ref.cast! :single) (:struct.get :single 0)
                 (:local.get 1) (:ref.cast! :single) (:struct.get :single 0) (:struct.new :complex-s) :return))
  '(:local.get 0) '(:local.get 1) '(:struct.new :complex))
(macrolet ((part (name field)
             `(defhelper ,name :e-e ()
                '(:block n1 :eqref
                  (:local.get 0) (:br_on_cast_fail n1 :eqref (:ref :complex-d)) (:struct.get :complex-d ,field)
                  (:struct.new :double) :return)
                :drop
                '(:block n2 :eqref
                  (:local.get 0) (:br_on_cast_fail n2 :eqref (:ref :complex-s)) (:struct.get :complex-s ,field)
                  (:struct.new :single) :return)
                :drop
                '(:local.get 0) '(:ref.cast :complex) '(:struct.get :complex ,field))))
  (part :complex-real 0)
  (part :complex-imag 1))
(defprim clysm-sys::%make-complex (r i) :value `(,@r ,@i (:call :make-complex)))
(defprim clysm-sys::%complex-real (c) :value `(,@c (:call :complex-real)))
(defprim clysm-sys::%complex-imag (c) :value `(,@c (:call :complex-imag)))
;; the parts of a complex number known to be a $complex-d / $complex-s (the
;; typed code generation keeps them unboxed)
(defprim clysm-sys::%cd-real (c) :value `(,@c (:ref.cast :complex-d) (:struct.get :complex-d 0) (:struct.new :double)))
(defprim clysm-sys::%cd-imag (c) :value `(,@c (:ref.cast :complex-d) (:struct.get :complex-d 1) (:struct.new :double)))
(defprim clysm-sys::%cs-real (c) :value `(,@c (:ref.cast :complex-s) (:struct.get :complex-s 0) (:struct.new :single)))
(defprim clysm-sys::%cs-imag (c) :value `(,@c (:ref.cast :complex-s) (:struct.get :complex-s 1) (:struct.new :single)))
(defprim clysm-sys::%make-complex-d (r i) :value
  `(,@r (:ref.cast :double) (:struct.get :double 0) ,@i (:ref.cast :double) (:struct.get :double 0)
    (:struct.new :complex-d)))
(defprim clysm-sys::%make-complex-s (r i) :value
  `(,@r (:ref.cast :single) (:struct.get :single 0) ,@i (:ref.cast :single) (:struct.get :single 0)
    (:struct.new :complex-s)))

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
;; a single that is an infinity (FLOAT and COERCE of a number beyond the singles)
(defprim clysm-sys::%single-infinite-p (s) :test
  `(,@s (:ref.cast :single) (:struct.get :single 0) :f32.abs (:f32.const #x7F800000)
    :f32.eq))
;; decode double: fields as fixnums
(defprim clysm-sys::%double-sign (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 63) :i64.shr_u :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-exponent (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 52) :i64.shr_u (:i64.const #x7FF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-mant-hi (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 26) :i64.shr_u (:i64.const #x3FFFFFF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-mant-lo (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const #x3FFFFFF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-mantissa (d) :value
  (let ((b (new-local :i64)))
    `(,@(f64-code d) :i64.reinterpret_f64 (:local.tee ,b) (:i64.const #xFFFFFFFFFFFFF) :i64.and
      (:i64.const #x10000000000000) (:i64.const 0)
      (:local.get ,b) (:i64.const 52) :i64.shr_u (:i64.const #x7FF) :i64.and :i64.eqz :i32.eqz :select
      :i64.or (:call :i64-int))))
(defprim clysm-sys::%make-double-from-parts (sign expo hi lo) :value
  ;; sign: 0/1, expo: raw 11 bits, hi: 26 bits, lo: 26 bits
  `(,@sign (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 63) :i64.shl
    ,@expo (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 52) :i64.shl :i64.or
    ,@hi (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 26) :i64.shl :i64.or
    ,@lo (:ref.cast :i31) :i31.get_s :i64.extend_i32_u :i64.or
    :f64.reinterpret_i64 ,@(box-f64)))

;;; The shortest digits of a float, by Schubfach (R. Giulietti, "The
;;; Schubfach way to render doubles", 2020; Java's Double.toString), with
;;; SBCL's choices where they differ: a subnormal is printed as SBCL's
;;; FLONUM-TO-DIGITS does, as if it had all the bits of a normal float (its
;;; significand shifted up to P bits, so that it gets up to 17 digits), and
;;; of two shortest decimals equally near, the larger is taken.  The table
;;; holds g = floor(10^-k 2^-r) + 1, r = floor(-k log2 10) - 125, as two
;;; halves of 63 bits, for k from +SCHUBFACH-KMIN+ (the powers below
;;; Schubfach's -324 are for the shifted subnormals) to 343 (the powers
;;; above 292 are for reading: %DECIMAL-FLOAT).
(defconstant +schubfach-kmin+ -345)
(defconstant +schubfach-kmax+ 343)
(defvar *schubfach-table-init* nil)
(defun schubfach-table-init ()
  (or *schubfach-table-init*
      (setf *schubfach-table-init*
            (let ((n 0) (code nil))
              (loop for k from +schubfach-kmin+ to +schubfach-kmax+
                    do (let* ((e (- k))
                              (r (- (ash (* e 913124641741) -38) 125))
                              (g (1+ (if (>= e 0)
                                         (if (>= r 0)
                                             (floor (expt 10 e) (expt 2 r))
                                             (* (expt 10 e) (expt 2 (- r))))
                                         (floor (expt 2 (- r)) (expt 10 (- e)))))))
                         (unless (<= (expt 2 125) g (1- (expt 2 126)))
                           (error "Schubfach table: g(~D) out of range" k))
                         (push `(:i64.const ,(ash g -63)) code)
                         (push `(:i64.const ,(ldb (byte 63 0) g)) code)
                         (incf n 2)))
              `(,@(nreverse code) (:array.new_fixed :u63vec ,n))))))

(defun schubfach-table-global ()
  "The global of the table, added to the module being compiled."
  (unless (find :schubfach-table *misc-globals* :key #'first)
    (push (list :schubfach-table '(:ref :u63vec) nil (schubfach-table-init)) *misc-globals*))
  :schubfach-table)

(defun mulhi64-code (a b)
  "The high 64 bits of the unsigned product of the i64 locals A and B."
  `((:local.get ,a) (:i64.const #xFFFFFFFF) :i64.and (:local.get ,b) (:i64.const #xFFFFFFFF) :i64.and :i64.mul
    (:local.set mll)
    (:local.get ,a) (:i64.const #xFFFFFFFF) :i64.and (:local.get ,b) (:i64.const 32) :i64.shr_u :i64.mul
    (:local.set mlh)
    (:local.get ,a) (:i64.const 32) :i64.shr_u (:local.get ,b) (:i64.const #xFFFFFFFF) :i64.and :i64.mul
    (:local.set mhl)
    (:local.get ,a) (:i64.const 32) :i64.shr_u (:local.get ,b) (:i64.const 32) :i64.shr_u :i64.mul
    (:local.get mlh) (:i64.const 32) :i64.shr_u :i64.add
    (:local.get mhl) (:i64.const 32) :i64.shr_u :i64.add
    (:local.get mll) (:i64.const 32) :i64.shr_u
    (:local.get mlh) (:i64.const #xFFFFFFFF) :i64.and :i64.add
    (:local.get mhl) (:i64.const #xFFFFFFFF) :i64.and :i64.add
    (:i64.const 32) :i64.shr_u :i64.add))

(defun schubfach-rop-code (cb dest)
  "DEST := the product of g (G1 and G0; G1 alone for a single) and CB << H,
rounded to odd (Schubfach's rop)."
  `((:local.get ,cb) (:local.get h) :i64.shl (:local.set cp)
    (:local.get single)
    (:if nil nil
         (,@(mulhi64-code 'g1 'cp) (:local.tee x1) (:i64.const 31) :i64.shr_u
          (:local.get x1) (:i64.const #xFFFFFFFF) :i64.and (:i64.const #xFFFFFFFF) :i64.add
          (:i64.const 32) :i64.shr_u :i64.or (:local.set ,dest))
         (,@(mulhi64-code 'g0 'cp) (:local.set x1)
          ;; z = (low(g1 cp) >> 1) + x1
          (:local.get g1) (:local.get cp) :i64.mul (:i64.const 1) :i64.shr_u (:local.get x1) :i64.add
          (:local.set z)
          ,@(mulhi64-code 'g1 'cp) (:local.get z) (:i64.const 63) :i64.shr_u :i64.add
          (:local.get z) (:i64.const #x7FFFFFFFFFFFFFFF) :i64.and (:i64.const #x7FFFFFFFFFFFFFFF) :i64.add
          (:i64.const 63) :i64.shr_u :i64.or (:local.set ,dest)))))

;;; (float, simple string) -> fixnum: the digits are stored in the string
;;; from index 0; the value is n + 32 negative-p + 64 (e + 2048) where
;;; |x| = 0.d1..dn 10^e (a zero: one digit 0, e = 1).
(defhelper :float-shortest :ee-e ((buf (:ref :null :string)) (single :i32) (neg :i32) (i :i32) (n :i32)
                                  (lo :i32) (hi :i32)
                                  (bits :i64) (bq :i64) (tm :i64) (p :i64) (qmin :i64) (cmin :i64)
                                  (c :i64) (q :i64) (k :i64) (h :i64) (out :i64) (cb :i64) (cbl :i64) (cbr :i64)
                                  (g1 :i64) (g0 :i64) (cp :i64) (x1 :i64) (z :i64)
                                  (mll :i64) (mlh :i64) (mhl :i64)
                                  (vb :i64) (vbl :i64) (vbr :i64) (s :i64) (tt :i64) (f :i64) (pw :i64))
  '(:local.get 1) '(:ref.cast :string) '(:local.set buf)
  '(:local.get 0) '(:ref.test :double)
  '(:if nil nil
    ((:local.get 0) (:ref.cast :double) (:struct.get :double 0) :i64.reinterpret_f64 (:local.tee bits)
     (:i64.const 63) :i64.shr_u :i32.wrap_i64 (:local.set neg)
     (:local.get bits) (:i64.const 52) :i64.shr_u (:i64.const #x7FF) :i64.and (:local.set bq)
     (:local.get bits) (:i64.const #xFFFFFFFFFFFFF) :i64.and (:local.set tm)
     (:i64.const 53) (:local.set p) (:i64.const -1074) (:local.set qmin)
     (:i64.const #x10000000000000) (:local.set cmin))
    ((:i32.const 1) (:local.set single)
     (:local.get 0) (:ref.cast :single) (:struct.get :single 0) :i32.reinterpret_f32 :i64.extend_i32_u
     (:local.tee bits)
     (:i64.const 31) :i64.shr_u :i32.wrap_i64 (:local.set neg)
     (:local.get bits) (:i64.const 23) :i64.shr_u (:i64.const #xFF) :i64.and (:local.set bq)
     (:local.get bits) (:i64.const #x7FFFFF) :i64.and (:local.set tm)
     (:i64.const 24) (:local.set p) (:i64.const -149) (:local.set qmin)
     (:i64.const #x800000) (:local.set cmin)))
  `(:block digits nil
     (:local.get bq) :i64.eqz
     (:if nil nil
          (;; zero
           (:local.get tm) :i64.eqz
           (:if nil nil ((:i64.const 0) (:local.set k) (:i64.const 0) (:local.set f) (:br digits)))
           ;; a subnormal: its significand shifted up to P bits, as SBCL does
           (:local.get p) (:i64.const 64) :i64.sub (:local.get tm) :i64.clz :i64.add (:local.set h)
           (:local.get tm) (:local.get h) :i64.shl (:local.set c)
           (:local.get qmin) (:local.get h) :i64.sub (:local.set q))
          ((:local.get cmin) (:local.get tm) :i64.or (:local.set c)
           (:local.get qmin) (:local.get bq) :i64.add (:i64.const 1) :i64.sub (:local.set q)))
     ;; an integer below 2^P: its digits
     (:local.get q) (:i64.const 0) :i64.lt_s
     (:local.get q) (:i64.const 0) (:local.get p) :i64.sub :i64.gt_s :i32.and
     (:if nil nil
          ((:local.get c) (:i64.const 1) (:i64.const 0) (:local.get q) :i64.sub :i64.shl (:i64.const 1) :i64.sub
           :i64.and :i64.eqz
           (:if nil nil
                ((:local.get c) (:i64.const 0) (:local.get q) :i64.sub :i64.shr_u (:local.set f)
                 (:i64.const 0) (:local.set k) (:br digits)))))
     (:local.get c) (:i64.const 1) :i64.and (:local.set out)
     (:local.get c) (:i64.const 2) :i64.shl (:local.tee cb) (:i64.const 2) :i64.add (:local.set cbr)
     (:local.get c) (:local.get cmin) :i64.ne (:local.get q) (:local.get qmin) :i64.eq :i32.or
     (:if nil nil
          ((:local.get cb) (:i64.const 2) :i64.sub (:local.set cbl)
           (:local.get q) (:i64.const 661971961083) :i64.mul (:i64.const 41) :i64.shr_s (:local.set k))
          ((:local.get cb) (:i64.const 1) :i64.sub (:local.set cbl)
           (:local.get q) (:i64.const 661971961083) :i64.mul (:i64.const -274743187321) :i64.add
           (:i64.const 41) :i64.shr_s (:local.set k)))
     ;; h = q + floor(-k log2 10) + (33 for a single, 2 for a double)
     (:local.get q) (:i64.const 0) (:local.get k) :i64.sub (:i64.const 913124641741) :i64.mul
     (:i64.const 38) :i64.shr_s :i64.add
     (:i64.const 33) (:i64.const 2) (:local.get single) :select :i64.add (:local.set h)
     (:global.get ,(schubfach-table-global))
     (:local.get k) (:i64.const ,(- +schubfach-kmin+)) :i64.add :i32.wrap_i64 (:i32.const 1) :i32.shl
     (:local.tee i)
     (:array.get :u63vec) (:local.set g1)
     (:local.get single)
     (:if nil nil
          ((:local.get g1) (:i64.const 1) :i64.add (:local.set g1))
          ((:global.get :schubfach-table) (:local.get i) (:i32.const 1) :i32.add (:array.get :u63vec)
           (:local.set g0)))
     ,@(schubfach-rop-code 'cb 'vb)
     ,@(schubfach-rop-code 'cbl 'vbl)
     ,@(schubfach-rop-code 'cbr 'vbr)
     (:local.get vb) (:i64.const 2) :i64.shr_u (:local.set s)
     ;; one digit less
     (:local.get s) (:i64.const 100) :i64.ge_u
     (:if nil nil
          ((:local.get s) (:i64.const 10) :i64.div_u (:i64.const 10) :i64.mul (:local.tee pw)
           (:i64.const 10) :i64.add (:local.set tt)
           (:local.get vbl) (:local.get out) :i64.add (:local.get pw) (:i64.const 2) :i64.shl :i64.le_u
           (:local.tee lo)
           (:local.get tt) (:i64.const 2) :i64.shl (:local.get out) :i64.add (:local.get vbr) :i64.le_u
           (:local.tee hi) :i32.ne
           (:if nil nil
                ((:local.get pw) (:local.get tt) (:local.get lo) :select (:local.set f)
                 (:br digits)))))
     (:local.get s) (:i64.const 1) :i64.add (:local.set tt)
     (:local.get vbl) (:local.get out) :i64.add (:local.get s) (:i64.const 2) :i64.shl :i64.le_u
     (:local.tee lo)
     (:local.get tt) (:i64.const 2) :i64.shl (:local.get out) :i64.add (:local.get vbr) :i64.le_u
     (:local.tee hi) :i32.ne
     (:if nil nil
          ((:local.get s) (:local.get tt) (:local.get lo) :select (:local.set f)
           (:br digits)))
     ;; both: the nearer, the larger when as near (as SBCL)
     (:local.get s) (:local.get tt)
     (:local.get vb) (:local.get s) (:local.get tt) :i64.add (:i64.const 1) :i64.shl :i64.sub
     (:i64.const 0) :i64.lt_s :select (:local.set f))
  ;; F * 10^K: the zeros at the end removed, the digits stored
  '(:local.get f) :i64.eqz
  `(:if nil nil
        ((:local.get buf) (:i32.const 0) (:i32.const 48) (:array.set :string)
         (:i32.const ,(+ 1 (* 64 2049))) (:local.get neg) (:i32.const 5) :i32.shl :i32.or
         :ref.i31 :return))
  '(:block zdone nil
    (:loop zagain nil
     (:local.get f) (:i64.const 10) :i64.rem_u :i64.eqz :i32.eqz (:br_if zdone)
     (:local.get f) (:i64.const 10) :i64.div_u (:local.set f)
     (:local.get k) (:i64.const 1) :i64.add (:local.set k)
     (:br zagain)))
  '(:i32.const 1) '(:local.set n) '(:i64.const 10) '(:local.set pw)
  '(:block ndone nil
    (:loop nagain nil
     (:local.get f) (:local.get pw) :i64.lt_u (:br_if ndone)
     (:local.get n) (:i32.const 1) :i32.add (:local.set n)
     (:local.get pw) (:i64.const 10) :i64.mul (:local.set pw)
     (:br nagain)))
  '(:local.get n) '(:local.set i)
  '(:loop wagain nil
    (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
    (:local.get buf) (:local.get i)
    (:local.get f) (:i64.const 10) :i64.rem_u :i32.wrap_i64 (:i32.const 48) :i32.add (:array.set :string)
    (:local.get f) (:i64.const 10) :i64.div_u (:local.set f)
    (:local.get i) (:br_if wagain))
  '(:local.get n) '(:local.get neg) '(:i32.const 5) :i32.shl :i32.or
  '(:local.get k) :i32.wrap_i64 '(:local.get n) :i32.add '(:i32.const 2048) :i32.add '(:i32.const 6) :i32.shl
  :i32.or
  :ref.i31)
(defprim clysm-sys::%float-shortest (x buf) :value
  (schubfach-table-global)
  `(,@x ,@buf (:call :float-shortest)))

;;; Reading a decimal: (string start end packed) -> the float nearest to
;;; the digits of STRING[START,END) (a point among them is skipped) times
;;; 10^e, where PACKED = 4e + 2 negative-p + single-p; NIL when the helper
;;; cannot tell (more than 19 significant digits, a result that is not a
;;; normal float, a product too near a half): the caller computes it exactly.
;;; Up to 2^53 (2^24 for a single) and 10^22 (10^10) the product of two
;;; exact floats is correctly rounded (Clinger); otherwise the digits W,
;;; shifted to 64 bits, times the 126 bits of 10^e of Schubfach's table
;;; give the top 128 bits of a product within 5 units of their last bit
;;; of the exact one, enough to round it unless its bits after the
;;; significand are within 8 of a half or of zero (as Eisel and Lemire's
;;; algorithm, with Schubfach's table).
(defhelper :decimal-float :eeee-e ((s (:ref :null :string)) (i :i32) (end :i32) (c :i32) (nd :i32)
                                   (single :i32) (neg :i32) (idx :i32)
                                   (w :i64) (q :i64) (lz :i64) (g1 :i64) (g0 :i64)
                                   (mll :i64) (mlh :i64) (mhl :i64)
                                   (ahi :i64) (alo :i64) (bhi :i64) (blo :i64) (uhi :i64) (ulo :i64)
                                   (sh :i64) (mask :i64) (mant :i64) (e2 :i64) (p :i64) (be :i64)
                                   (d :f64) (pw :f64) (fs :f32) (fpw :f32))
  '(:local.get 0) '(:ref.cast :string) '(:local.set s)
  '(:local.get 1) '(:ref.cast :i31) :i31.get_s '(:local.set i)
  '(:local.get 2) '(:ref.cast :i31) :i31.get_s '(:local.set end)
  '(:local.get 3) '(:ref.cast :i31) :i31.get_s '(:local.tee c) '(:i32.const 1) :i32.and '(:local.set single)
  '(:local.get c) '(:i32.const 1) :i32.shr_u '(:i32.const 1) :i32.and '(:local.set neg)
  '(:local.get c) '(:i32.const 2) :i32.shr_s :i64.extend_i32_s '(:local.set q)
  ;; the significant digits
  `(:block ddone nil
    (:loop dagain nil
     (:local.get i) (:local.get end) :i32.ge_s (:br_if ddone)
     (:local.get s) (:local.get i) (:array.get :string) (:local.set c)
     (:local.get i) (:i32.const 1) :i32.add (:local.set i)
     (:local.get c) (:i32.const 46) :i32.eq (:br_if dagain)
     (:local.get c) (:i32.const 48) :i32.sub (:local.tee c)
     (:local.get w) :i64.eqz :i32.eqz :i32.or
     (:if nil nil
          ((:local.get nd) (:i32.const 1) :i32.add (:local.tee nd) (:i32.const 19) :i32.gt_u
           (:if nil nil (,@(nil-code) :return))
           (:local.get w) (:i64.const 10) :i64.mul (:local.get c) :i64.extend_i32_u :i64.add (:local.set w)))
     (:br dagain)))
  ;; zero
  '(:local.get w) :i64.eqz
  '(:if nil nil
    ((:local.get single)
     (:if nil nil
          ((:f32.const 0f0) (:f32.const -1f0) (:f32.const 1f0) (:local.get neg) :select :f32.copysign
           (:struct.new :single) :return)
          ((:f64.const 0d0) (:f64.const -1d0) (:f64.const 1d0) (:local.get neg) :select :f64.copysign
           (:struct.new :double) :return))))
  ;; Clinger's fast path
  '(:local.get single)
  '(:if nil nil
    ((:local.get w) (:i64.const #x1000000) :i64.le_u
     (:local.get q) (:i64.const -10) :i64.ge_s :i32.and (:local.get q) (:i64.const 10) :i64.le_s :i32.and
     (:if nil nil
          ((:f32.const 1f0) (:local.set fpw)
           (:local.get q) (:i64.const 0) :i64.lt_s
           (:if nil nil ((:i64.const 0) (:local.get q) :i64.sub (:local.set lz)) ((:local.get q) (:local.set lz)))
           (:block pdone nil
             (:loop pagain nil
               (:local.get lz) :i64.eqz (:br_if pdone)
               (:local.get fpw) (:f32.const 10f0) :f32.mul (:local.set fpw)
               (:local.get lz) (:i64.const 1) :i64.sub (:local.set lz)
               (:br pagain)))
           (:local.get w) :f32.convert_i64_u (:local.set fs)
           (:local.get q) (:i64.const 0) :i64.lt_s
           (:if nil :f32 ((:local.get fs) (:local.get fpw) :f32.div) ((:local.get fs) (:local.get fpw) :f32.mul))
           (:local.set fs)
           (:local.get fs) :f32.neg (:local.get fs) (:local.get neg) :select (:struct.new :single) :return)))
    ((:local.get w) (:i64.const #x20000000000000) :i64.le_u
     (:local.get q) (:i64.const -22) :i64.ge_s :i32.and (:local.get q) (:i64.const 22) :i64.le_s :i32.and
     (:if nil nil
          ((:f64.const 1d0) (:local.set pw)
           (:local.get q) (:i64.const 0) :i64.lt_s
           (:if nil nil ((:i64.const 0) (:local.get q) :i64.sub (:local.set lz)) ((:local.get q) (:local.set lz)))
           (:block pdone nil
             (:loop pagain nil
               (:local.get lz) :i64.eqz (:br_if pdone)
               (:local.get pw) (:f64.const 10d0) :f64.mul (:local.set pw)
               (:local.get lz) (:i64.const 1) :i64.sub (:local.set lz)
               (:br pagain)))
           (:local.get w) :f64.convert_i64_u (:local.set d)
           (:local.get q) (:i64.const 0) :i64.lt_s
           (:if nil :f64 ((:local.get d) (:local.get pw) :f64.div) ((:local.get d) (:local.get pw) :f64.mul))
           (:local.set d)
           (:local.get d) :f64.neg (:local.get d) (:local.get neg) :select (:struct.new :double) :return))))
  ;; the powers of the table
  `(:local.get q) `(:i64.const ,(- +schubfach-kmax+)) :i64.lt_s
  '(:local.get q) '(:i64.const 308) :i64.gt_s :i32.or
  `(:if nil nil (,@(nil-code) :return))
  `(:global.get ,(schubfach-table-global))
  `(:i64.const ,(- +schubfach-kmin+)) '(:local.get q) :i64.sub :i32.wrap_i64 '(:i32.const 1) :i32.shl '(:local.tee idx)
  '(:array.get :u63vec) '(:local.set g1)
  '(:global.get :schubfach-table) '(:local.get idx) '(:i32.const 1) :i32.add '(:array.get :u63vec) '(:local.set g0)
  '(:local.get w) :i64.clz '(:local.set lz) '(:local.get w) '(:local.get lz) :i64.shl '(:local.set w)
  ;; A = w g1, B = w g0; U = 2A + floor(B / 2^62) = floor(w g / 2^62)
  '(:local.get w) '(:local.get g1) :i64.mul '(:local.set alo)
  (mulhi64-code 'w 'g1) '(:local.set ahi)
  '(:local.get w) '(:local.get g0) :i64.mul '(:local.set blo)
  (mulhi64-code 'w 'g0) '(:local.set bhi)
  '(:local.get alo) '(:i64.const 1) :i64.shl
  '(:local.get bhi) '(:i64.const 2) :i64.shl '(:local.get blo) '(:i64.const 62) :i64.shr_u :i64.or
  :i64.add '(:local.set ulo)
  '(:local.get ahi) '(:i64.const 1) :i64.shl '(:local.get alo) '(:i64.const 63) :i64.shr_u :i64.or
  '(:local.get bhi) '(:i64.const 62) :i64.shr_u :i64.add
  ;; (the carry of the low halves)
  '(:local.get ulo) '(:local.get bhi) '(:i64.const 2) :i64.shl '(:local.get blo) '(:i64.const 62) :i64.shr_u :i64.or
  :i64.lt_u :i64.extend_i32_u :i64.add '(:local.set uhi)
  ;; U has 127 or 128 bits: the significand and a rounding bit are in UHI
  '(:i64.const 24) '(:i64.const 53) '(:local.get single) :select '(:local.set p)
  '(:i64.const 62) '(:local.get p) :i64.sub '(:local.get uhi) '(:i64.const 63) :i64.shr_u :i64.add '(:local.set sh)
  '(:i64.const 1) '(:local.get sh) :i64.shl '(:i64.const 1) :i64.sub '(:local.set mask)
  ;; the bits below: too near zero or all ones, the rounding is not known
  '(:local.get uhi) '(:local.get mask) :i64.and '(:local.tee mant) :i64.eqz
  '(:local.get ulo) '(:i64.const 8) :i64.lt_u :i32.and
  '(:local.get mant) '(:local.get mask) :i64.eq
  '(:local.get ulo) '(:i64.const -9) :i64.gt_u :i32.and :i32.or
  `(:if nil nil (,@(nil-code) :return))
  '(:local.get uhi) '(:local.get sh) :i64.shr_u '(:i64.const 1) :i64.add '(:i64.const 1) :i64.shr_u '(:local.set mant)
  ;; e2 = sh + 127 + r - lz, r = floor(q log2 10) - 125
  '(:local.get sh) '(:i64.const 2) :i64.add
  '(:local.get q) '(:i64.const 913124641741) :i64.mul '(:i64.const 38) :i64.shr_s :i64.add
  '(:local.get lz) :i64.sub '(:local.set e2)
  '(:local.get mant) '(:local.get p) :i64.shr_u :i64.eqz :i32.eqz
  '(:if nil nil ((:local.get mant) (:i64.const 1) :i64.shr_u (:local.set mant)
                 (:local.get e2) (:i64.const 1) :i64.add (:local.set e2)))
  ;; the biased exponent
  '(:local.get e2) '(:local.get p) :i64.add '(:i64.const 126) '(:i64.const 1022) '(:local.get single) :select
  :i64.add '(:local.tee be) '(:i64.const 1) :i64.lt_s
  '(:local.get be) '(:i64.const 254) '(:i64.const 2046) '(:local.get single) :select :i64.gt_s :i32.or
  `(:if nil nil (,@(nil-code) :return))
  '(:local.get single)
  '(:if nil :eqref
    ((:local.get be) (:i64.const 23) :i64.shl (:local.get mant) (:i64.const #x7FFFFF) :i64.and :i64.or
     (:local.get neg) :i64.extend_i32_u (:i64.const 31) :i64.shl :i64.or :i32.wrap_i64 :f32.reinterpret_i32
     (:struct.new :single))
    ((:local.get be) (:i64.const 52) :i64.shl (:local.get mant) (:i64.const #xFFFFFFFFFFFFF) :i64.and :i64.or
     (:local.get neg) :i64.extend_i32_u (:i64.const 63) :i64.shl :i64.or :f64.reinterpret_i64
     (:struct.new :double))))
(defprim clysm-sys::%decimal-float (str start end packed) :value
  (schubfach-table-global)
  `(,@str ,@start ,@end ,@packed (:call :decimal-float)))

;;; (string start end negative-p) -> the integer of the decimal digits
;;; STRING[START,END): up to 18 digits in an i64, more in limbs multiplied
;;; by 10^9 and added a group of nine digits at a time.
(defhelper :decimal-int :eeee-e ((s (:ref :null :string)) (i :i32) (end :i32) (n :i32) (k :i32) (used :i32)
                                 (j :i32) (m (:ref :null :limbs)) (w :i64) (mult :i64) (tt :i64))
  '(:local.get 0) '(:ref.cast :string) '(:local.set s)
  '(:local.get 1) '(:ref.cast :i31) :i31.get_s '(:local.set i)
  '(:local.get 2) '(:ref.cast :i31) :i31.get_s '(:local.tee end) '(:local.get i) :i32.sub '(:local.tee n)
  '(:i32.const 18) :i32.le_s
  `(:if nil nil
    ((:block done nil
       (:loop again nil
         (:local.get i) (:local.get end) :i32.ge_s (:br_if done)
         (:local.get w) (:i64.const 10) :i64.mul
         (:local.get s) (:local.get i) (:array.get :string) (:i32.const 48) :i32.sub :i64.extend_i32_u :i64.add
         (:local.set w)
         (:local.get i) (:i32.const 1) :i32.add (:local.set i)
         (:br again)))
     (:i64.const 0) (:local.get w) :i64.sub (:local.get w) (:local.get 3) ,@(nil-code) :ref.eq :i32.eqz :select
     (:call :i64-int) :return))
  ;; limbs: 32 bits hold more than nine digits
  '(:local.get n) '(:i32.const 9) :i32.div_u '(:i32.const 2) :i32.add '(:array.new_default :limbs) '(:local.set m)
  ;; the first group has N mod 9 digits (9 when none)
  '(:local.get n) '(:i32.const 9) :i32.rem_u '(:local.tee k) :i32.eqz
  '(:if nil nil ((:i32.const 9) (:local.set k)))
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get end) :i32.ge_s (:br_if done)
       (:i64.const 0) (:local.set w) (:i64.const 1) (:local.set mult)
       (:block gdone nil
         (:loop gagain nil
           (:local.get k) :i32.eqz (:br_if gdone)
           (:local.get w) (:i64.const 10) :i64.mul
           (:local.get s) (:local.get i) (:array.get :string) (:i32.const 48) :i32.sub :i64.extend_i32_u :i64.add
           (:local.set w)
           (:local.get mult) (:i64.const 10) :i64.mul (:local.set mult)
           (:local.get i) (:i32.const 1) :i32.add (:local.set i)
           (:local.get k) (:i32.const 1) :i32.sub (:local.set k)
           (:br gagain)))
       (:i32.const 9) (:local.set k)
       ;; m := m * mult + w
       (:i32.const 0) (:local.set j)
       (:block mdone nil
         (:loop magain nil
           (:local.get j) (:local.get used) :i32.ge_u (:br_if mdone)
           (:local.get m) (:local.get j) (:array.get :limbs) :i64.extend_i32_u (:local.get mult) :i64.mul
           (:local.get w) :i64.add (:local.tee tt)
           (:i64.const ,+limb-bits+) :i64.shr_u (:local.set w)
           (:local.get m) (:local.get j) (:local.get tt) :i32.wrap_i64 (:array.set :limbs)
           (:local.get j) (:i32.const 1) :i32.add (:local.set j)
           (:br magain)))
       (:local.get w) :i64.eqz :i32.eqz
       (:if nil nil ((:local.get m) (:local.get used) (:local.get w) :i32.wrap_i64 (:array.set :limbs)
                     (:local.get used) (:i32.const 1) :i32.add (:local.set used)))
       (:br again)))
  '(:local.get m) '(:local.get 3) (nil-code) :ref.eq :i32.eqz '(:call :make-int))
(defprim clysm-sys::%decimal-integer (str start end neg) :value
  `(,@str ,@start ,@end ,@neg (:call :decimal-int)))

;;; PARSE-INTEGER of a simple string in one call: (string start end radix
;;; junk-allowed) -> the integer (NIL when JUNK-ALLOWED and there are no
;;; digits) and the index, as PARSE-INTEGER's two values; the symbol
;;; %PARSE-INTEGER-FAIL when the Lisp function must do it (other arguments,
;;; an error to signal, digits beyond 63 bits in a radix other than 10).
(defun parse-integer-fail-code ()
  `((:global.get ,(sym-global 'clysm-sys::%parse-integer-fail)) :return))
(defhelper :parse-integer :eeeee-e ((s (:ref :null :string)) (i :i32) (e :i32) (r :i32) (c :i32) (d :i32)
                                     (neg :i32) (ds :i32) (de :i32) (big :i32) (v :i64) (limit :i64)
                                     (val :eqref))
  ;; the arguments
  '(:local.get 0) '(:ref.test :string) :i32.eqz
  '(:local.get 1) '(:ref.test :i31) :i32.eqz :i32.or
  '(:local.get 3) '(:ref.test :i31) :i32.eqz :i32.or
  `(:if nil nil ,(parse-integer-fail-code))
  '(:local.get 0) '(:ref.cast :string) '(:local.tee s) :array.len '(:local.set e)
  '(:local.get 1) '(:ref.cast :i31) :i31.get_s '(:local.set i)
  '(:local.get 2) (nil-code) :ref.eq :i32.eqz
  `(:if nil nil
        ((:local.get 2) (:ref.test :i31) :i32.eqz (:if nil nil ,(parse-integer-fail-code))
         (:local.get 2) (:ref.cast :i31) :i31.get_s (:local.tee c) (:local.get e) :i32.gt_u
         (:if nil nil ,(parse-integer-fail-code))
         (:local.get c) (:local.set e)))
  '(:local.get i) '(:local.get e) :i32.gt_u
  '(:local.get 3) '(:ref.cast :i31) :i31.get_s '(:local.tee r) '(:i32.const 2) :i32.lt_s :i32.or
  '(:local.get r) '(:i32.const 36) :i32.gt_s :i32.or
  `(:if nil nil ,(parse-integer-fail-code))
  '(:i64.const #x7FFFFFFFFFFFFFFF) '(:local.get r) :i64.extend_i32_u :i64.div_u '(:local.set limit)
  ;; whitespace, a sign
  (flet ((skip-whitespace ()
           '(:block wdone nil
             (:loop wagain nil
              (:local.get i) (:local.get e) :i32.ge_u (:br_if wdone)
              (:local.get s) (:local.get i) (:array.get :string) (:local.tee c) (:i32.const 32) :i32.eq
              (:local.get c) (:i32.const 9) :i32.sub (:i32.const 5) :i32.lt_u
              (:local.get c) (:i32.const 11) :i32.ne :i32.and :i32.or
              :i32.eqz (:br_if wdone)
              (:local.get i) (:i32.const 1) :i32.add (:local.set i)
              (:br wagain)))))
    (list
     (skip-whitespace)
     '(:local.get i) '(:local.get e) :i32.lt_u
     '(:if nil nil
       ((:local.get s) (:local.get i) (:array.get :string) (:local.tee c) (:i32.const 45) :i32.eq
        (:if nil nil ((:i32.const 1) (:local.set neg) (:local.get i) (:i32.const 1) :i32.add (:local.set i))
             ((:local.get c) (:i32.const 43) :i32.eq
              (:if nil nil ((:local.get i) (:i32.const 1) :i32.add (:local.set i)))))))
     '(:local.get i) '(:local.set ds)
     ;; the digits
     '(:block ddone nil
       (:loop dagain nil
        (:local.get i) (:local.get e) :i32.ge_u (:br_if ddone)
        (:local.get s) (:local.get i) (:array.get :string) (:local.tee c) (:i32.const 48) :i32.sub
        (:local.tee d) (:i32.const 10) :i32.ge_u
        (:if nil nil
             ((:i32.const 99) (:local.set d)
              (:local.get c) (:i32.const 32) :i32.or (:i32.const 97) :i32.sub (:local.tee c) (:i32.const 26) :i32.lt_u
              (:if nil nil ((:local.get c) (:i32.const 10) :i32.add (:local.set d)))))
        (:local.get d) (:local.get r) :i32.ge_u (:br_if ddone)
        (:local.get v) (:local.get limit) :i64.ge_u
        (:if nil nil ((:i32.const 1) (:local.set big))
             ((:local.get v) (:local.get r) :i64.extend_i32_u :i64.mul (:local.get d) :i64.extend_i32_u :i64.add
              (:local.set v)))
        (:local.get i) (:i32.const 1) :i32.add (:local.set i)
        (:br dagain)))
     '(:local.get i) '(:local.set de)
     ;; the value (NIL without digits)
     '(:local.get de) '(:local.get ds) :i32.eq
     `(:if nil nil
           (,@(nil-code) (:local.set val))
           ((:local.get big)
            (:if nil nil
                 ((:local.get r) (:i32.const 10) :i32.ne (:if nil nil ,(parse-integer-fail-code))
                  (:local.get 0) (:local.get ds) :ref.i31 (:local.get de) :ref.i31
                  (:local.get neg) (:if nil :eqref ,(t-code) ,(nil-code))
                  (:call :decimal-int) (:local.set val))
                 ((:i64.const 0) (:local.get v) :i64.sub (:local.get v) (:local.get neg) :select
                  (:call :i64-int) (:local.set val)))))
     ;; with JUNK-ALLOWED the index is that of the character after the digits
     '(:local.get val) (car (nil-code)) :ref.eq '(:local.get 4) (car (nil-code)) :ref.eq :i32.or
     `(:if nil nil ,(list (skip-whitespace)))
     ;; junk (or no digits) without JUNK-ALLOWED: the Lisp function signals
     '(:local.get 4) (car (nil-code)) :ref.eq
     '(:local.get val) (car (nil-code)) :ref.eq '(:local.get i) '(:local.get e) :i32.ne :i32.or :i32.and
     `(:if nil nil ,(parse-integer-fail-code))
     '(:global.get :mv-buf) '(:i32.const 0) '(:local.get i) :ref.i31 '(:array.set :vector)
     '(:i32.const 2) '(:global.set :mv-count)
     '(:local.get val))))
(defprim clysm-sys::%parse-integer-fast (string start end radix junk) :mv
  `(,@string ,@start ,@end ,@radix ,@junk (:call :parse-integer)))
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
(defprim clysm-sys::%js-wrap-async-fn (f) :value
  `(,@f (:ref.cast :closure) :extern.convert_any (:call :js-wrap-async-fn) ,@(wrap-ext)))
(defprim clysm-sys::%js-host (k) :value `(,@(idx-code k) (:call :js-host) ,@(wrap-ext)))
(defprim clysm-sys::%js-await (o) :value `(,@(ext-code o) (:call :js-await) ,@(wrap-ext)))
(defprim clysm-sys::%js-truthy (o) :test `(,@(ext-code o) (:call :js-truthy)))
(defprim clysm-sys::%js-lisp-object-p (o) :test `(,@(ext-code o) (:call :js-is-lisp)))
(defprim clysm-sys::%js-wrap-lisp (x) :value `(,@x :extern.convert_any ,@(wrap-ext)))
(defprim clysm-sys::%js-unwrap-lisp (o) :value `(,@(ext-code o) :any.convert_extern (:ref.cast :null :eq)))

;;; Strings and octets cross to the host at once, through the linear
;;; memory that the image exports (from offset 0): a string as UTF-16 code
;;; units, octets as they are.

(defun mem-ensure-code (bytes-local)
  "Grow the memory to hold the number of bytes in BYTES-LOCAL (an i32 local)."
  `((:local.get ,bytes-local) :memory.size (:i32.const 16) :i32.shl :i32.gt_u
    (:if nil nil
         ((:local.get ,bytes-local) (:i32.const 65535) :i32.add (:i32.const 16) :i32.shr_u
          :memory.size :i32.sub :memory.grow :drop))))

;; the characters of the simple string S from START to END as UTF-16 code
;; units; the value is the number of bytes written, plus 1 if a code unit is
;; a surrogate of its own (not a pair)
(defprim clysm-sys::%mem-put-string (s start end) :value
  (let ((str (new-local '(:ref :null :string))) (i (new-local :i32)) (e (new-local :i32))
        (p (new-local :i32)) (c (new-local :i32)) (flag (new-local :i32)) (n (new-local :i32))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@s (:ref.cast :string) (:local.set ,str)
      ,@(idx-code start) (:local.set ,i) ,@(idx-code end) (:local.set ,e)
      (:local.get ,e) (:local.get ,i) :i32.sub (:i32.const 2) :i32.shl (:local.set ,n)
      ,@(mem-ensure-code n)
      (:i32.const 0) (:local.set ,p) (:i32.const 0) (:local.set ,flag)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,i) (:local.get ,e) :i32.ge_s (:br_if ,done)
          (:local.get ,str) (:local.get ,i) (:array.get :string) (:local.set ,c)
          (:local.get ,c) (:i32.const #x10000) :i32.ge_u
          (:if nil nil
               ((:local.get ,c) (:i32.const #x10000) :i32.sub (:local.set ,c)
                (:local.get ,p) (:local.get ,c) (:i32.const 10) :i32.shr_u (:i32.const #xD800) :i32.or
                (:i32.store16 0)
                (:local.get ,p) (:local.get ,c) (:i32.const #x3FF) :i32.and (:i32.const #xDC00) :i32.or
                (:i32.store16 2)
                (:local.get ,p) (:i32.const 4) :i32.add (:local.set ,p))
               ((:local.get ,c) (:i32.const #xF800) :i32.and (:i32.const #xD800) :i32.eq
                (:if nil nil ((:i32.const 1) (:local.set ,flag)))
                (:local.get ,p) (:local.get ,c) (:i32.store16 0)
                (:local.get ,p) (:i32.const 2) :i32.add (:local.set ,p)))
          (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
          (:br ,again)))
      (:local.get ,p) (:local.get ,flag) :i32.or :ref.i31)))

;; a new simple string of the N UTF-16 code units at the start of the memory
(defprim clysm-sys::%mem-get-string (n) :value
  (let ((units (new-local :i32)) (k (new-local :i32)) (cnt (new-local :i32)) (u (new-local :i32))
        (str (new-local '(:ref :null :string))) (done (gensym "DONE")) (again (gensym "AGAIN"))
        (done2 (gensym "DONE")) (again2 (gensym "AGAIN")))
    (flet ((pair-test ()
             ;; u is a high surrogate and the next unit a low one
             `((:local.get ,u) (:i32.const #xFC00) :i32.and (:i32.const #xD800) :i32.eq
               (:if nil :i32
                    ((:local.get ,k) (:i32.const 1) :i32.add (:local.get ,units) :i32.lt_u
                     (:if nil :i32
                          ((:local.get ,k) (:i32.const 1) :i32.shl (:i32.load16_u 2)
                           (:i32.const #xFC00) :i32.and (:i32.const #xDC00) :i32.eq)
                          ((:i32.const 0))))
                    ((:i32.const 0))))))
      `(,@(idx-code n) (:local.set ,units)
        (:i32.const 0) (:local.set ,k) (:i32.const 0) (:local.set ,cnt)
        (:block ,done nil
          (:loop ,again nil
            (:local.get ,k) (:local.get ,units) :i32.ge_u (:br_if ,done)
            (:local.get ,k) (:i32.const 1) :i32.shl (:i32.load16_u 0) (:local.set ,u)
            ,@(pair-test)
            (:if nil nil
                 ((:local.get ,k) (:i32.const 2) :i32.add (:local.set ,k))
                 ((:local.get ,k) (:i32.const 1) :i32.add (:local.set ,k)))
            (:local.get ,cnt) (:i32.const 1) :i32.add (:local.set ,cnt)
            (:br ,again)))
        (:local.get ,cnt) (:array.new_default :string) (:local.set ,str)
        (:i32.const 0) (:local.set ,k) (:i32.const 0) (:local.set ,cnt)
        (:block ,done2 nil
          (:loop ,again2 nil
            (:local.get ,k) (:local.get ,units) :i32.ge_u (:br_if ,done2)
            (:local.get ,k) (:i32.const 1) :i32.shl (:i32.load16_u 0) (:local.set ,u)
            (:local.get ,str) (:local.get ,cnt)
            ,@(pair-test)
            (:if nil :i32
                 ((:local.get ,u) (:i32.const #x3FF) :i32.and (:i32.const 10) :i32.shl
                  (:local.get ,k) (:i32.const 1) :i32.shl (:i32.load16_u 2) (:i32.const #x3FF) :i32.and
                  :i32.or (:i32.const #x10000) :i32.add
                  (:local.get ,k) (:i32.const 2) :i32.add (:local.set ,k))
                 ((:local.get ,u)
                  (:local.get ,k) (:i32.const 1) :i32.add (:local.set ,k)))
            (:array.set :string)
            (:local.get ,cnt) (:i32.const 1) :i32.add (:local.set ,cnt)
            (:br ,again2)))
        (:local.get ,str) (:ref.as_non_null)))))

;; the octets of the simple (unsigned-byte 8) vector V from START to END;
;; the value is their number
(defprim clysm-sys::%mem-put-octets (v start end) :value
  (let ((b (new-local '(:ref :null :bytes))) (i (new-local :i32)) (e (new-local :i32)) (n (new-local :i32))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@v (:ref.cast :bytes) (:local.set ,b)
      ,@(idx-code start) (:local.set ,i) ,@(idx-code end) (:local.set ,e)
      (:local.get ,e) (:local.get ,i) :i32.sub (:local.set ,n)
      ,@(mem-ensure-code n)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,i) (:local.get ,e) :i32.ge_s (:br_if ,done)
          (:local.get ,i) (:local.get ,e) :i32.sub (:local.get ,n) :i32.add
          (:local.get ,b) (:local.get ,i) (:array.get_u :bytes)
          (:i32.store8 0)
          (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
          (:br ,again)))
      (:local.get ,n) :ref.i31)))

;; a new simple (unsigned-byte 8) vector of the N octets at the start of the memory
(defprim clysm-sys::%mem-get-octets (n) :value
  (let ((len (new-local :i32)) (k (new-local :i32)) (b (new-local '(:ref :null :bytes)))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@(idx-code n) (:local.set ,len)
      (:local.get ,len) (:array.new_default :bytes) (:local.set ,b)
      (:i32.const 0) (:local.set ,k)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,k) (:local.get ,len) :i32.ge_u (:br_if ,done)
          (:local.get ,b) (:local.get ,k) (:local.get ,k) (:i32.load8_u 0) (:array.set :bytes)
          (:local.get ,k) (:i32.const 1) :i32.add (:local.set ,k)
          (:br ,again)))
      (:local.get ,b) (:ref.as_non_null))))

(defprim clysm-sys::%js-str-from-mem (info) :value `(,@(idx-code info) (:call :js-str-from-mem) ,@(wrap-ext)))
(defprim clysm-sys::%js-str-to-mem (o) :value `(,@(ext-code o) (:call :js-str-to-mem) :ref.i31))
(defprim clysm-sys::%js-octets-from-mem (n) :value `(,@(idx-code n) (:call :js-octets-from-mem) ,@(wrap-ext)))
(defprim clysm-sys::%js-octets-to-mem (o) :value `(,@(ext-code o) (:call :js-octets-to-mem) :ref.i31))
(defprim clysm-sys::%js-read-file-mem (o) :value `(,@(ext-code o) (:call :js-read-file-mem) :ref.i31))

;;; UTF-8 between simple strings and simple (unsigned-byte 8) vectors, in
;;; one Wasm loop (file streams, runtime/stream.lisp)

;; a new simple string of the UTF-8 octets of BYTES from START to END (a
;; malformed sequence is U+FFFD)
;; (the decoder: pass 0 counts the characters, pass 1 stores them)
(defprim clysm-sys::%utf8-decode (bytes start end) :value
  (let* ((b (new-local '(:ref :null :bytes))) (i (new-local :i32)) (e (new-local :i32)) (n (new-local :i32))
         (c (new-local :i32)) (k (new-local :i32)) (more (new-local :i32))
         (str (new-local '(:ref :null :string))) (pass (new-local :i32)) (j (new-local :i32)) (s0 (new-local :i32))
         (done (gensym "DONE")) (again (gensym "AGAIN")) (inner (gensym "INNER")) (inner-done (gensym "IDONE"))
         (passes (gensym "PASSES")) (passes-done (gensym "PDONE")))
    (labels ((lead (limit count mask else)
               ;; a first octet of COUNT continuations: keep its MASK bits
               `((:local.get ,c) (:i32.const ,limit) :i32.ge_u
                 (:if nil nil
                      ((:i32.const ,count) (:local.set ,more)
                       (:local.get ,c) (:i32.const ,mask) :i32.and (:local.set ,c))
                      ,else)))
             (bad () `((:i32.const #xFFFD) (:local.set ,c) (:br ,inner-done))))
      (let* ((classify (lead #xF0 3 7
                             (lead #xE0 2 15
                                   (lead #xC0 1 31
                                         `((:i32.const 0) (:local.set ,more) (:i32.const #xFFFD) (:local.set ,c))))))
             (continuations
               `((:i32.const 0) (:local.set ,j)
                 (:block ,inner-done nil
                   (:loop ,inner nil
                     (:local.get ,j) (:local.get ,more) :i32.ge_u (:br_if ,inner-done)
                     (:local.get ,i) (:local.get ,e) :i32.ge_u
                     (:if nil nil ,(bad))
                     (:local.get ,b) (:local.get ,i) (:array.get_u :bytes) (:local.set ,k)
                     (:local.get ,k) (:i32.const #xC0) :i32.and (:i32.const #x80) :i32.ne
                     (:if nil nil ,(bad))
                     (:local.get ,c) (:i32.const 6) :i32.shl
                     (:local.get ,k) (:i32.const #x3F) :i32.and :i32.or (:local.set ,c)
                     (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
                     (:local.get ,j) (:i32.const 1) :i32.add (:local.set ,j)
                     (:br ,inner)))
                 (:local.get ,c) (:i32.const #x10FFFF) :i32.gt_u
                 (:if nil nil ((:i32.const #xFFFD) (:local.set ,c)))))
             (one-character
               `((:local.get ,b) (:local.get ,i) (:array.get_u :bytes) (:local.set ,c)
                 (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
                 (:local.get ,c) (:i32.const #x80) :i32.ge_u
                 (:if nil nil (,@classify ,@continuations))
                 (:local.get ,pass)
                 (:if nil nil ((:local.get ,str) (:local.get ,n) (:local.get ,c) (:array.set :string)))
                 (:local.get ,n) (:i32.const 1) :i32.add (:local.set ,n))))
        `(,@bytes (:ref.cast :bytes) (:local.set ,b)
          ,@(idx-code start) (:local.set ,s0) ,@(idx-code end) (:local.set ,e)
          (:i32.const 0) (:local.set ,pass)
          (:block ,passes-done nil
            (:loop ,passes nil
              (:local.get ,s0) (:local.set ,i) (:i32.const 0) (:local.set ,n)
              (:block ,done nil
                (:loop ,again nil
                  (:local.get ,i) (:local.get ,e) :i32.ge_u (:br_if ,done)
                  ,@one-character
                  (:br ,again)))
              (:local.get ,pass) (:br_if ,passes-done)
              (:local.get ,n) (:array.new_default :string) (:local.set ,str)
              (:i32.const 1) (:local.set ,pass)
              (:br ,passes)))
          (:local.get ,str) (:ref.as_non_null))))))

;; encode the characters of the simple string STRING from START to END in
;; UTF-8 into BYTES at POS (which has room for them); the value is the
;; position after them
(defprim clysm-sys::%utf8-encode-into (string start end bytes pos) :value
  (let* ((str (new-local '(:ref :null :string))) (b (new-local '(:ref :null :bytes)))
         (i (new-local :i32)) (e (new-local :i32)) (p (new-local :i32)) (c (new-local :i32))
         (done (gensym "DONE")) (again (gensym "AGAIN")))
    (labels ((put (value-code offset)
               (append `((:local.get ,b) (:local.get ,p))
                       (if (zerop offset) nil `((:i32.const ,offset) :i32.add))
                       value-code
                       '((:array.set :bytes))))
             (lead (shift tag)
               `((:local.get ,c) (:i32.const ,shift) :i32.shr_u (:i32.const ,tag) :i32.or))
             (cont (shift)
               `((:local.get ,c) ,@(if (zerop shift) nil `((:i32.const ,shift) :i32.shr_u))
                 (:i32.const #x3F) :i32.and (:i32.const #x80) :i32.or))
             (advance (n) `((:local.get ,p) (:i32.const ,n) :i32.add (:local.set ,p))))
      (let ((one (append (put `((:local.get ,c)) 0) (advance 1)))
            (two (append (put (lead 6 #xC0) 0) (put (cont 0) 1) (advance 2)))
            (three (append (put (lead 12 #xE0) 0) (put (cont 6) 1) (put (cont 0) 2) (advance 3)))
            (four (append (put (lead 18 #xF0) 0) (put (cont 12) 1) (put (cont 6) 2) (put (cont 0) 3) (advance 4))))
        `(,@string (:ref.cast :string) (:local.set ,str) ,@(idx-code start) (:local.set ,i)
          ,@(idx-code end) (:local.set ,e)
          ,@bytes (:ref.cast :bytes) (:local.set ,b) ,@(idx-code pos) (:local.set ,p)
          (:block ,done nil
            (:loop ,again nil
              (:local.get ,i) (:local.get ,e) :i32.ge_s (:br_if ,done)
              (:local.get ,str) (:local.get ,i) (:array.get :string) (:local.set ,c)
              (:local.get ,c) (:i32.const #x80) :i32.lt_u
              (:if nil nil ,one
                   ((:local.get ,c) (:i32.const #x800) :i32.lt_u
                    (:if nil nil ,two
                         ((:local.get ,c) (:i32.const #x10000) :i32.lt_u
                          (:if nil nil ,three ,four)))))
              (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
              (:br ,again)))
          (:local.get ,p) :ref.i31)))))

;; the index of the last newline of the simple string STRING between START
;; and END, or -1
(defprim clysm-sys::%string-last-newline (string start end) :value
  (let ((str (new-local '(:ref :null :string))) (i (new-local :i32)) (s0 (new-local :i32))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@string (:ref.cast :string) (:local.set ,str) ,@(idx-code start) (:local.set ,s0)
      ,@(idx-code end) (:i32.const 1) :i32.sub (:local.set ,i)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,i) (:local.get ,s0) :i32.lt_s (:br_if ,done)
          (:local.get ,str) (:local.get ,i) (:array.get :string) (:i32.const 10) :i32.eq (:br_if ,done)
          (:local.get ,i) (:i32.const 1) :i32.sub (:local.set ,i)
          (:br ,again)))
      (:local.get ,i) (:local.get ,s0) :i32.lt_s
      (:if nil :i32 ((:i32.const -1)) ((:local.get ,i))) :ref.i31)))

;; the next character of the stream S when it is an open character input
;; file (runtime/stream.lisp: its layout is that of FILE-STREAM and its
;; FAST slot is :FILE) and that character is ASCII, else NIL; with ADVANCE
;; true it is read (READ-CHAR), otherwise only looked at (PEEK-CHAR).  The
;; slot indices are those checked by stream.lisp when it is loaded.
(defparameter *fs-slot-fast* 14)
(defprim clysm-sys::%fs-fast-char (s advance) :value
  ;; a file: its FAST slot holds its $fsbuf (a type that only file streams
  ;; hold); a string input stream: its layout and FAST :STRING
  (let* ((o (new-local)) (sl (new-local '(:ref :null :vector))) (p (new-local :i32)) (b (new-local :i32))
         (f (new-local)) (fb (new-local '(:ref :null :fsbuf))) (no (gensym "NO")) (out (gensym "OUT"))
         ;; ADVANCE is T or NIL at the call site: decided here
         (adv (cond ((equal advance (nil-code)) :no) ((equal advance (t-code)) :yes) (t :test)))
         (step-file `((:local.get ,fb) (:local.get ,p) (:i32.const 1) :i32.add (:struct.set :fsbuf 1)))
         (step-string `((:local.get ,sl) (:i32.const 3) (:local.get ,p) (:i32.const 1) :i32.add :ref.i31
                        (:array.set :vector))))
    (flet ((maybe (step)
             (ecase adv
               (:no nil)
               (:yes step)
               (:test `(,@advance ,@(nil-code) :ref.eq (:if nil nil nil ,step))))))
    `((:block ,out :eqref
        (:block ,no nil
          ,@s (:local.tee ,o) (:ref.test :instance) :i32.eqz (:br_if ,no)
          (:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 1) (:local.tee ,sl)
          :array.len (:i32.const ,*fs-slot-fast*) :i32.le_u (:br_if ,no)
          (:local.get ,sl) (:i32.const ,*fs-slot-fast*) (:array.get :vector) (:local.tee ,f)
          (:ref.test :fsbuf)
          (:if nil nil
               ((:local.get ,f) (:ref.cast! :fsbuf) (:local.tee ,fb) (:struct.get :fsbuf 1) (:local.tee ,p)
                (:local.get ,fb) (:struct.get :fsbuf 2) :i32.ge_s (:br_if ,no)
                (:local.get ,fb) (:struct.get :fsbuf 0) (:local.get ,p) (:array.get_u :bytes)
                (:local.tee ,b) (:i32.const 128) :i32.ge_u (:br_if ,no)
                ,@(maybe step-file)
                (:local.get ,b) (:i32.const ,+char-tag+) :i32.add :ref.i31 (:br ,out)))
          ;; a string input stream of a simple string (FAST is :STRING)
          (:local.get ,f) (:global.get ,(sym-global :string)) :ref.eq :i32.eqz (:br_if ,no)
          (:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 0)
          (:global.get ,(sym-global 'clysm-sys::*string-stream-layout*)) (:struct.get :symbol ,+sym-value+)
          :ref.eq :i32.eqz (:br_if ,no)
          (:local.get ,sl) (:i32.const 3) (:array.get :vector) (:ref.cast :i31) :i31.get_s (:local.tee ,p)
          (:local.get ,sl) (:i32.const 4) (:array.get :vector) (:ref.cast :i31) :i31.get_s
          :i32.ge_s (:br_if ,no)
          ,@(maybe step-string)
          (:local.get ,sl) (:i32.const 2) (:array.get :vector) (:ref.cast :string) (:local.get ,p)
          (:array.get :string) (:i32.const ,+char-tag+) :i32.add :ref.i31 (:br ,out))
        ,@(nil-code))))))

;; the index of the first octet BYTE of the simple (unsigned-byte 8) vector
;; BYTES between START and END, or END
(defprim clysm-sys::%bytes-find (bytes byte start end) :value
  (let ((b (new-local '(:ref :null :bytes))) (x (new-local :i32)) (i (new-local :i32)) (e (new-local :i32))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@bytes (:ref.cast :bytes) (:local.set ,b) ,@(idx-code byte) (:local.set ,x)
      ,@(idx-code start) (:local.set ,i) ,@(idx-code end) (:local.set ,e)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,i) (:local.get ,e) :i32.ge_s (:br_if ,done)
          (:local.get ,b) (:local.get ,i) (:array.get_u :bytes) (:local.get ,x) :i32.eq (:br_if ,done)
          (:local.get ,i) (:i32.const 1) :i32.add (:local.set ,i)
          (:br ,again)))
      (:local.get ,i) :ref.i31)))

;; a file stream's buffer ($fsbuf: octets, position, number of valid octets)
(defprim clysm-sys::%make-fsbuf (octets pos fill) :value
  `(,@octets (:ref.cast :bytes) ,@(idx-code pos) ,@(idx-code fill) (:struct.new :fsbuf)))
(defprim clysm-sys::%fsbuf-octets (b) :value `(,@b (:ref.cast :fsbuf) (:struct.get :fsbuf 0) (:ref.as_non_null)))
(defprim clysm-sys::%fsbuf-pos (b) :value `(,@b (:ref.cast :fsbuf) (:struct.get :fsbuf 1) :ref.i31))
(defprim clysm-sys::%fsbuf-fill (b) :value `(,@b (:ref.cast :fsbuf) (:struct.get :fsbuf 2) :ref.i31))
(defprim clysm-sys::%set-fsbuf-octets (b v) :value
  (let ((tmp (new-local)))
    `(,@b (:ref.cast :fsbuf) ,@v (:local.tee ,tmp) (:ref.cast :bytes) (:struct.set :fsbuf 0) (:local.get ,tmp))))
(defprim clysm-sys::%set-fsbuf-pos (b v) :value
  (let ((tmp (new-local)))
    `(,@b (:ref.cast :fsbuf) ,@v (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s (:struct.set :fsbuf 1) (:local.get ,tmp))))
(defprim clysm-sys::%set-fsbuf-fill (b v) :value
  (let ((tmp (new-local)))
    `(,@b (:ref.cast :fsbuf) ,@v (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s (:struct.set :fsbuf 2) (:local.get ,tmp))))
(defprim clysm-sys::%fsbufp (x) :test `(,@x (:ref.test :fsbuf)))

;; exported entry used by JS to call Lisp closures
(defhelper :lisp-apply :xx-x ()
  '(:local.get 0) '(:struct.new :foreign) '(:local.get 1) '(:struct.new :foreign)
  `(:global.get ,(sym-global 'clysm-sys::%js-callback)) '(:call (:callsym 2))
  '(:ref.cast :foreign) '(:struct.get :foreign 0))
;; after an exception that Lisp cannot catch (the stack exhausted, a trap)
;; went out of a call from JavaScript: undo what it skipped
(defhelper :lisp-recover :i-v ()
  '(:local.get 0) :ref.i31
  `(:global.get ,(sym-global 'clysm-sys::%js-recover)) '(:call (:callsym 1)) :drop)
;; the same for a function of JS:ASYNC-FUNCTION (called through WebAssembly.promising)
(defhelper :lisp-apply-async :xx-x ()
  '(:local.get 0) '(:struct.new :foreign) '(:local.get 1) '(:struct.new :foreign)
  `(:global.get ,(sym-global 'clysm-sys::%js-async-callback)) '(:call (:callsym 2))
  '(:ref.cast :foreign) '(:struct.get :foreign 0))

(defnative vector (form env)
  `(:wasm :eqref ,(convert-args (cdr form) env)
          ((:array.new_fixed :vector ,(length (cdr form))))))

(defhelper :eval-string :x-x ()
  '(:local.get 0) '(:struct.new :foreign)
  `(:global.get ,(sym-global 'clysm-sys::%host-eval-string)) '(:call (:callsym 1))
  '(:ref.cast :foreign) '(:struct.get :foreign 0))

;;; ------------------------------------------------------------
;;; Specialized storage (layout.lisp *storage-kinds*): every kind of simple
;;; one-dimensional array is handled by the same helpers.
;;; ------------------------------------------------------------

(defun typed-storage-kinds () (cddr *storage-kinds*))   ; all but :vector and :string
(defun storage-local (kind) (intern (format nil "S-~A" kind) :clysm))
(defun storage-locals (kinds)
  (loop for k in kinds collect (list (storage-local (first k)) `(:ref :null ,(first k)))))

(defun storage-get-code (kind)
  "Instructions (after the array and an i32 index) leaving the element raw."
  (destructuring-bind (name et wt lo hi) (storage-kind-info kind)
    (declare (ignore et hi))
    (list (list (cond ((member wt '(:i8 :i16)) (if (minusp lo) :array.get_s :array.get_u))
                      (t :array.get))
                name))))

(defun storage-raw-type (kind)
  "The Wasm type of a raw element."
  (let ((wt (third (storage-kind-info kind))))
    (if (member wt '(:i8 :i16)) :i32 wt)))

(defun storage-box-code (kind)
  "Instructions boxing a raw element of KIND."
  (ecase kind
    (:vector nil)
    (:string `((:i32.const ,+char-tag+) :i32.add :ref.i31))
    ((:bits :u7vec :bytes :s8vec :u15vec :u16vec :s16vec) '(:ref.i31))
    (:u32vec '(:i64.extend_i32_u (:call :i64-int)))
    ((:u31vec :s32vec) '((:call :box-i32)))
    (:u64vec '((:call :u64-int)))
    ((:u63vec :s64vec) '((:call :i64-int)))
    (:f32vec '((:struct.new :single)))
    (:f64vec '((:struct.new :double)))))

(defun storage-unbox-code (kind x raw fail)
  "Helper-body code leaving in local RAW the raw element for the eqref in
local X, doing FAIL (instructions) when X is not of the element type."
  (destructuring-bind (name et wt lo hi) (storage-kind-info kind)
    (declare (ignore name et))
    (ecase kind
      (:vector `((:local.get ,x) (:local.set ,raw)))
      (:string `((:local.get ,x) (:ref.test :i31) :i32.eqz (:if nil nil ,fail)
                 (:local.get ,x) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.sub (:local.tee ,raw)
                 (:i32.const 0) :i32.lt_s (:if nil nil ,fail)))
      ((:bits :u7vec :bytes :s8vec :u15vec :u16vec :s16vec)
       `((:local.get ,x) (:ref.test :i31) :i32.eqz (:if nil nil ,fail)
         (:local.get ,x) (:ref.cast! :i31) :i31.get_s (:local.tee ,raw)
         (:i32.const ,lo) :i32.sub (:i32.const ,(- hi lo)) :i32.gt_u (:if nil nil ,fail)))
      ((:u31vec :u32vec :s32vec)
       `((:local.get ,x) (:call :small-int-p) :i32.eqz (:if nil nil ,fail)
         (:local.get ,x) (:call :int-i64) (:local.tee w)
         (:i64.const ,lo) :i64.sub (:i64.const ,(- hi lo)) :i64.gt_u (:if nil nil ,fail)
         (:local.get w) :i32.wrap_i64 (:local.set ,raw)))
      ((:u63vec :u64vec :s64vec)
       ;; (integers of more than 56 bits with digits go through the Lisp function)
       `((:local.get ,x) (:call :small-int-p) :i32.eqz (:if nil nil ,fail)
         (:local.get ,x) (:call :int-i64) (:local.set ,raw)
         ,@(unless (eq kind :s64vec) `((:local.get ,raw) (:i64.const 0) :i64.lt_s (:if nil nil ,fail)))))
      (:f32vec (progn wt `((:local.get ,x) (:ref.test :single) :i32.eqz (:if nil nil ,fail)
                           (:local.get ,x) (:ref.cast! :single) (:struct.get :single 0) (:local.set ,raw))))
      (:f64vec `((:local.get ,x) (:ref.test :double) :i32.eqz (:if nil nil ,fail)
                 (:local.get ,x) (:ref.cast! :double) (:struct.get :double 0) (:local.set ,raw))))))

(defun storage-raw-local (kind)
  (ecase (storage-raw-type kind) (:eqref 'raw-e) (:i32 'raw-i) (:i64 'raw-l) (:f32 'raw-f) (:f64 'raw-d)))
(defparameter *storage-raw-locals* '((raw-e :eqref) (raw-i :i32) (raw-l :i64) (raw-f :f32) (raw-d :f64) (w :i64)))

(defhelper :u64-int :l-e ()
  '(:local.get 0) '(:i64.const 0) :i64.ge_s '(:if nil nil ((:local.get 0) (:call :i64-int) :return))
  '(:local.get 0) '(:i64.const 32) :i64.shr_u '(:call :i64-int)
  '(:local.get 0) '(:i64.const #xFFFFFFFF) :i64.and '(:call :i64-int)
  (lisp-call-instrs 'clysm-sys::%u64-from-halves 2))

(defun storage-dispatch-code (kinds arg fn &optional (result :eqref))
  "Helper-body code: for each of KINDS, when local ARG is such a storage, the
instructions (FN kind typed-local), which must return or leave nothing."
  (declare (ignorable result))
  (loop for k in kinds
        for kind = (first k)
        for next = (gensym "NEXT")
        collect `(:block ,next :eqref
                   (:local.get ,arg) (:br_on_cast_fail ,next :eqref (:ref ,kind)) (:local.set ,(storage-local kind))
                   ,@(funcall fn kind (storage-local kind))
                   (:local.get ,arg))
        collect :drop))

;; element I of a specialized storage; null when local 0 is not one or I is
;; out of bounds
(defhelper* :typed-ref :ei-e (storage-locals (typed-storage-kinds))
  (storage-dispatch-code
   (typed-storage-kinds) 0
   (lambda (kind l)
     `((:local.get ,l) :array.len (:local.get 1) :i32.gt_u :i32.eqz (:if nil nil ((:ref.null :eq) :return))
       (:local.get ,l) (:local.get 1) ,@(storage-get-code kind) ,@(storage-box-code kind) :return)))
  '(:ref.null :eq))

;; store local 2 in element I of a specialized storage; 0 when local 0 is not
;; one, I is out of bounds or the value is not of the element type
(defhelper* :typed-set :eie-i (append (storage-locals (typed-storage-kinds)) *storage-raw-locals*)
  (storage-dispatch-code
   (typed-storage-kinds) 0
   (lambda (kind l)
     (let ((raw (storage-raw-local kind)))
       `((:local.get ,l) :array.len (:local.get 1) :i32.gt_u :i32.eqz (:if nil nil ((:i32.const 0) :return))
         ,@(storage-unbox-code kind 2 raw '((:i32.const 0) :return))
         (:local.get ,l) (:local.get 1) (:local.get ,raw) (:array.set ,kind) (:i32.const 1) :return))))
  '(:i32.const 0))

;; element I of any storage (the Lisp function signals the errors)
(defhelper :storage-ref :ei-e ((v (:ref :null :vector)) (s (:ref :null :string)) (r :eqref))
  `(:block notvec :eqref
     (:local.get 0) (:br_on_cast_fail notvec :eqref (:ref :vector)) (:local.tee v)
     :array.len (:local.get 1) :i32.gt_u
     (:if nil nil ((:local.get v) (:local.get 1) (:array.get :vector) :return))
     (:local.get 0))
  :drop
  `(:block notstr :eqref
     (:local.get 0) (:br_on_cast_fail notstr :eqref (:ref :string)) (:local.tee s)
     :array.len (:local.get 1) :i32.gt_u
     (:if nil nil ((:local.get s) (:local.get 1) (:array.get :string)
                   (:i32.const ,+char-tag+) :i32.add :ref.i31 :return))
     (:local.get 0))
  :drop
  '(:local.get 0) '(:local.get 1) '(:call :typed-ref) '(:local.tee r) :ref.is_null :i32.eqz
  '(:if nil nil ((:local.get r) :return))
  '(:local.get 0) '(:local.get 1) :ref.i31
  (lisp-call-instrs 'clysm-sys::%simple-ref-error 2))

(defhelper :storage-set :eie-e ((v (:ref :null :vector)) (s (:ref :null :string)) (c :i32))
  `(:block notvec :eqref
     (:local.get 0) (:br_on_cast_fail notvec :eqref (:ref :vector)) (:local.tee v)
     :array.len (:local.get 1) :i32.gt_u
     (:if nil nil ((:local.get v) (:local.get 1) (:local.get 2) (:array.set :vector) (:local.get 2) :return))
     (:local.get 0))
  :drop
  `(:block notstr :eqref
     (:local.get 0) (:br_on_cast_fail notstr :eqref (:ref :string)) (:local.tee s)
     :array.len (:local.get 1) :i32.gt_u
     (:local.get 2) (:ref.test :i31) :i32.and
     (:if nil nil
          ((:local.get 2) (:ref.cast! :i31) :i31.get_s (:local.tee c) (:i32.const ,+char-tag+) :i32.ge_s
           (:if nil nil ((:local.get s) (:local.get 1) (:local.get c) (:i32.const ,+char-tag+) :i32.sub
                         (:array.set :string) (:local.get 2) :return))))
     (:local.get 0))
  :drop
  '(:local.get 0) '(:local.get 1) '(:local.get 2) '(:call :typed-set)
  '(:if nil nil ((:local.get 2) :return))
  '(:local.get 0) '(:local.get 1) :ref.i31 '(:local.get 2)
  (lisp-call-instrs 'clysm-sys::%simple-set-error 3))

;; the length of a storage, -1 for other objects
(defhelper* :storage-len :e-i (storage-locals *storage-kinds*)
  (storage-dispatch-code *storage-kinds* 0 (lambda (kind l) (declare (ignore kind)) `((:local.get ,l) :array.len :return)))
  '(:i32.const -1))

;; the kind (index in *storage-kinds*) of a storage, -1 for other objects
(defhelper :storage-kind :e-i ()
  (loop for k in *storage-kinds* for code from 0
        append `((:local.get 0) (:ref.test ,(first k)) (:if nil nil ((:i32.const ,code) :return))))
  '(:i32.const -1))

;; a fresh specialized storage of kind (code) local 0 with local 1 elements (zeros)
(defhelper :make-storage :ii-e ()
  (loop for k in (typed-storage-kinds)
        append `((:local.get 0) (:i32.const ,(storage-kind-code (first k))) :i32.eq
                 (:if nil nil ((:local.get 1) (:array.new_default ,(first k)) :return))))
  :unreachable)

;; fill elements [local 2, local 3) of storage local 0 with local 1; 0 when
;; that is not possible (not a storage, bad bounds, value not of the element type)
(defhelper* :storage-fill :eeii-i (append (storage-locals *storage-kinds*) *storage-raw-locals*)
  '(:local.get 2) '(:local.get 3) :i32.gt_u '(:if nil nil ((:i32.const 0) :return))
  (storage-dispatch-code
   *storage-kinds* 0
   (lambda (kind l)
     (let ((raw (storage-raw-local kind)))
       `((:local.get ,l) :array.len (:local.get 3) :i32.lt_u (:if nil nil ((:i32.const 0) :return))
         ,@(storage-unbox-code kind 1 raw '((:i32.const 0) :return))
         (:local.get ,l) (:local.get 2) (:local.get ,raw) (:local.get 3) (:local.get 2) :i32.sub
         (:array.fill ,kind) (:i32.const 1) :return))))
  '(:i32.const 0))

;; copy local 4 elements from storage local 2 at local 3 to storage local 0 at
;; local 1 (the regions may overlap); 0 when the storages are not of the same
;; kind or a region is out of bounds
(defhelper* :storage-copy :eieii-i (append (storage-locals *storage-kinds*) '((n :i32)))
  (storage-dispatch-code
   *storage-kinds* 0
   (lambda (kind l)
     `((:local.get 2) (:ref.test ,kind) :i32.eqz (:if nil nil ((:i32.const 0) :return))
       (:local.get 1) (:i32.const 0) :i32.lt_s (:local.get 3) (:i32.const 0) :i32.lt_s :i32.or
       (:local.get 4) (:i32.const 0) :i32.lt_s :i32.or (:if nil nil ((:i32.const 0) :return))
       (:local.get ,l) :array.len (:local.get 1) :i32.sub (:local.get 4) :i32.lt_s (:if nil nil ((:i32.const 0) :return))
       (:local.get 2) (:ref.cast! ,kind) :array.len (:local.get 3) :i32.sub (:local.get 4) :i32.lt_s
       (:if nil nil ((:i32.const 0) :return))
       (:local.get ,l) (:local.get 1) (:local.get 2) (:ref.cast! ,kind) (:local.get 3) (:local.get 4)
       (:array.copy ,kind ,kind) (:i32.const 1) :return)))
  '(:i32.const 0))

(defprim clysm-sys::%simple-ref (v i) :value `(,@v ,@(idx-code i) (:call :storage-ref)))
(defprim clysm-sys::%simple-set (v i x) :value `(,@v ,@(idx-code i) ,@x (:call :storage-set)))
(defprim clysm-sys::%storage-length (v) :value `(,@v (:call :storage-len) :ref.i31))
(defprim clysm-sys::%storage-kind (v) :value `(,@v (:call :storage-kind) :ref.i31))
(defprim clysm-sys::%make-typed-storage (code n) :value `(,@(idx-code code) ,@(idx-code n) (:call :make-storage)))
(defprim clysm-sys::%typed-set (v i x) :test `(,@v ,@(idx-code i) ,@x (:call :typed-set)))
(defprim clysm-sys::%storage-fill (v x start end) :test
  `(,@v ,@x ,@(idx-code start) ,@(idx-code end) (:call :storage-fill)))
(defprim clysm-sys::%storage-copy (dst di src si n) :test
  `(,@dst ,@(idx-code di) ,@src ,@(idx-code si) ,@(idx-code n) (:call :storage-copy)))
;; the two 32-bit halves of a 64-bit element (for integers with digits)
(defprim clysm-sys::%set-64vec-halves (v i hi lo) :value
  (let ((a (new-local)) (k (new-local :i32)) (w (new-local :i64)))
    `(,@v (:local.set ,a) ,@(idx-code i) (:local.set ,k)
      ,@hi (:call :int-i64) (:i64.const 32) :i64.shl ,@lo (:call :int-i64) :i64.or (:local.set ,w)
      (:local.get ,a) (:ref.test :u64vec)
      (:if nil nil ((:local.get ,a) (:ref.cast! :u64vec) (:local.get ,k) (:local.get ,w) (:array.set :u64vec))
           ((:local.get ,a) (:ref.test :u63vec)
            (:if nil nil ((:local.get ,a) (:ref.cast! :u63vec) (:local.get ,k) (:local.get ,w) (:array.set :u63vec))
                 ((:local.get ,a) (:ref.cast :s64vec) (:local.get ,k) (:local.get ,w) (:array.set :s64vec)))))
      ,@(nil-code))))

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
         :drop
         ;; a specialized vector (array headers are handled below)
         (:local.get 0) (:ref.test :mdarray) :i32.eqz
         (:if nil nil
              ((:local.get 0) (:local.get i) (:call :typed-ref) (:local.tee d) :ref.is_null :i32.eqz
               (:if nil nil ((:local.get d) :return))))))
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
         :drop
         (:local.get 0) (:ref.test :string) (:local.get 0) (:ref.test :mdarray) :i32.or :i32.eqz
         (:if nil nil ((:local.get 0) (:local.get i) (:local.get 2) (:call :typed-set)
                       (:if nil nil ((:local.get 2) :return))))))
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
;; the dimension of a rank-1 dimension list, else 0
(defhelper :dims-1d :e-i ()
  '(:local.get 0) '(:ref.test :cons) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :cons) '(:struct.get :cons 1) (car (nil-code)) :ref.eq :i32.eqz
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :cons) '(:struct.get :cons 0) '(:ref.test :i31) :i32.eqz
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :cons) '(:struct.get :cons 0) '(:ref.cast! :i31) :i31.get_s)

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
;; (SETF SVREF): a simple vector and an index within it inline, else the Lisp function (which signals)
(defhelper :svset :eee-e ((v (:ref :null :vector)) (i :i32))
  '(:local.get 1) '(:ref.test :i31)
  `(:if nil nil
        ((:block notvec :eqref
           (:local.get 0) (:br_on_cast_fail notvec :eqref (:ref :vector)) (:local.tee v)
           :array.len (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee i) :i32.gt_u
           (:if nil nil ((:local.get v) (:local.get i) (:local.get 2) (:array.set :vector)
                         (:local.get 2) :return))
           (:local.get 0))
         :drop))
  '(:local.get 0) '(:local.get 1) '(:local.get 2)
  `(:global.get ,(sym-global 'clysm-sys::%svset)) '(:call (:callsym 3)))
(defprim clysm-sys::%svset (v i x) :value `(,@v ,@i ,@x (:call :svset)))
;; (inline in typed.lisp when V is known to be a simple vector; otherwise a
;; simple vector and a fixnum index within it inline, the function, which
;; signals, for anything else)
(defprim svref (v i) :value
  (let ((tv (new-local)) (ti (new-local)) (vec (new-local '(:ref :null :vector))) (k (new-local :i32))
        (slow (gensym "SLOW")) (done (gensym "SVREF")))
    `(,@v (:local.set ,tv) ,@i (:local.set ,ti)
      (:block ,done :eqref
        (:block ,slow nil
          (:local.get ,ti) (:ref.test :i31) :i32.eqz (:br_if ,slow)
          (:local.get ,tv) (:ref.test :vector) :i32.eqz (:br_if ,slow)
          (:local.get ,tv) (:ref.cast! :vector) (:local.tee ,vec) :array.len
          (:local.get ,ti) (:ref.cast! :i31) :i31.get_s (:local.tee ,k) :i32.le_u (:br_if ,slow)
          (:local.get ,vec) (:local.get ,k) (:array.get :vector) (:br ,done))
        (:local.get ,tv) (:local.get ,ti) ,@(lisp-call-instrs 'svref 2)))))

;;; ------------------------------------------------------------
;;; Irrational functions
;;; ------------------------------------------------------------
;;; SIN COS TAN ASIN ACOS ATAN EXP LOG SINH COSH TANH ASINH ACOSH ATANH of
;;; one argument are primitives: for a double, a single or an integer within
;;; 64 bits the helper calls the host function and returns a float of the
;;; argument's format (a single for an integer).  A result that is not
;;; finite (outside the domain: the result is complex; an overflow) and
;;; every other argument go to the Lisp function %NAME-SLOW (an underflow is
;;; no condition: the zero or denormalized result).
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
           ((:local.get dr) (:struct.new :double) :return))
      (:local.get dr) :f32.demote_f64 (:local.tee fr) (:local.get fr) :f32.sub (:f32.const 0) :f32.ne (:br_if slow)
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

;;; The hash of a bignum: over the base 2^32 digits of its magnitude, h := (31 h
;;; + digit) mod 2^28 starting from the sign (the same value whether the
;;; integer holds its value or limbs; nothing is allocated)
(defhelper :bignum-hash :e-e ((b (:ref :null :bignum)) (h :i32) (w :i64) (m (:ref :null :limbs)) (i :i32) (n :i32))
  '(:local.get 0) '(:ref.cast :bignum) '(:local.tee b) '(:struct.get :bignum 0) '(:local.set h)
  '(:local.get b) '(:struct.get :bignum 2) '(:local.tee w) :i64.eqz
  `(:if nil nil
        (;; limbs
         (:local.get b) (:struct.get :bignum 1) (:local.tee m) :ref.is_null (:if nil nil ((:local.get h) :ref.i31 :return))
         (:local.get m) :array.len (:local.set n)
         (:block done nil
           (:loop again nil
             (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
             (:local.get h) (:i32.const 31) :i32.mul (:local.get m) (:local.get i) (:array.get :limbs) :i32.add
             (:i32.const #xFFFFFFF) :i32.and (:local.set h)
             (:local.get i) (:i32.const 1) :i32.add (:local.set i)
             (:br again))))
        (;; the digits of |value|
         (:local.get w) (:i64.const 0) :i64.lt_s (:if nil nil ((:i64.const 0) (:local.get w) :i64.sub (:local.set w)))
         (:block done2 nil
           (:loop again2 nil
             (:local.get w) :i64.eqz (:br_if done2)
             (:local.get h) (:i32.const 31) :i32.mul (:local.get w) :i32.wrap_i64 :i32.add
             (:i32.const #xFFFFFFF) :i32.and (:local.set h)
             (:local.get w) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set w)
             (:br again2)))))
  '(:local.get h) :ref.i31)
(defprim clysm-sys::%bignum-hash (x) :value `(,@x (:call :bignum-hash)))

;;; The digits of a fixnum or an integer within 64 bits in base 2 .. 36 as a
;;; string; NIL for other integers
(defhelper :int64-string :ei-e ((v :i32) (xa :i64) (ka :i32) (m :i64) (tt :i64) (b :i64) (n :i32) (neg :i32) (d :i32)
                                (str (:ref :null :string)))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     (:local.get 1) :i64.extend_i32_u (:local.set b)
     (:local.get xa) (:i64.const 0) :i64.lt_s (:local.tee neg)
     (:if nil :i64 ((:i64.const 0) (:local.get xa) :i64.sub) ((:local.get xa))) (:local.set m)
     ;; the number of digits
     (:i32.const 1) (:local.set n) (:local.get m) (:local.set tt)
     (:block cdone nil
       (:loop cagain nil
         (:local.get tt) (:local.get b) :i64.div_u (:local.tee tt) :i64.eqz (:br_if cdone)
         (:local.get n) (:i32.const 1) :i32.add (:local.set n)
         (:br cagain)))
     (:local.get n) (:local.get neg) :i32.add (:local.tee n) (:array.new_default :string) (:local.set str)
     (:local.get neg) (:if nil nil ((:local.get str) (:i32.const 0) (:i32.const 45) (:array.set :string)))
     (:block fdone nil
       (:loop fagain nil
         (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
         (:local.get m) (:local.get b) :i64.rem_u :i32.wrap_i64 (:local.set d)
         (:local.get str) (:local.get n)
         (:i32.const 48) (:i32.const 55) (:local.get d) (:i32.const 10) :i32.lt_u :select (:local.get d) :i32.add
         (:array.set :string)
         (:local.get m) (:local.get b) :i64.div_u (:local.tee m) :i64.eqz (:br_if fdone)
         (:br fagain)))
     (:local.get str) :ref.as_non_null :return)
  (car (nil-code)))
(defprim clysm-sys::%int64-to-string (x base) :value `(,@x ,@(idx-code base) (:call :int64-string)))
;;; The decimal digits of an integer of any size (with a minus sign) as a
;;; fresh string: a copy of the magnitude is divided in place by 10^9, each
;;; remainder giving nine digits, written from the end of a buffer.
(defhelper :int-decimal :e-e ((m (:ref :null :limbs)) (src (:ref :null :limbs)) (buf (:ref :null :string))
                              (s (:ref :null :string)) (n :i32) (pos :i32) (size :i32) (i :i32) (k :i32)
                              (rem :i64) (cur :i64) (q :i64) (r :i32))
  '(:local.get 0) '(:call :int-limbs) '(:ref.cast! :limbs) '(:local.tee src) '(:call :mag-len) '(:local.set n)
  '(:local.get n) '(:array.new_default :limbs) '(:local.set m)
  '(:local.get m) '(:i32.const 0) '(:local.get src) '(:i32.const 0) '(:local.get n) '(:array.copy :limbs :limbs)
  ;; (a digit of 32 bits is less than ten decimal digits)
  '(:local.get n) '(:i32.const 10) :i32.mul '(:i32.const 2) :i32.add '(:local.tee size) '(:local.tee pos)
  '(:array.new_default :string) '(:local.set buf)
  `(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       ;; m := m / 10^9, rem := the remainder
       (:i64.const 0) (:local.set rem) (:local.get n) (:local.set i)
       (:block ddone nil
         (:loop dagain nil
           (:local.get i) :i32.eqz (:br_if ddone)
           (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
           (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
           (:local.get m) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.tee cur)
           (:i64.const 1000000000) :i64.div_u (:local.set q)
           (:local.get m) (:local.get i) (:local.get q) :i32.wrap_i64 (:array.set :limbs)
           (:local.get cur) (:local.get q) (:i64.const 1000000000) :i64.mul :i64.sub (:local.set rem)
           (:br dagain)))
       (:local.get m) (:local.get n) (:i32.const 1) :i32.sub (:array.get :limbs) :i32.eqz
       (:if nil nil ((:local.get n) (:i32.const 1) :i32.sub (:local.set n)))
       ;; nine digits (the last chunk: without leading zeros)
       (:local.get rem) :i32.wrap_i64 (:local.set r) (:i32.const 0) (:local.set k)
       (:block wdone nil
         (:loop wagain nil
           (:local.get k) (:i32.const 9) :i32.ge_u (:br_if wdone)
           (:local.get n) :i32.eqz (:local.get r) :i32.eqz :i32.and (:br_if wdone)
           (:local.get pos) (:i32.const 1) :i32.sub (:local.set pos)
           (:local.get buf) (:local.get pos) (:local.get r) (:i32.const 10) :i32.rem_u (:i32.const 48) :i32.add
           (:array.set :string)
           (:local.get r) (:i32.const 10) :i32.div_u (:local.set r)
           (:local.get k) (:i32.const 1) :i32.add (:local.set k)
           (:br wagain)))
       (:br again)))
  ;; zero
  '(:local.get pos) '(:local.get size) :i32.eq
  '(:if nil nil ((:local.get pos) (:i32.const 1) :i32.sub (:local.set pos)
                 (:local.get buf) (:local.get pos) (:i32.const 48) (:array.set :string)))
  '(:local.get 0) '(:call :int-neg)
  '(:if nil nil ((:local.get pos) (:i32.const 1) :i32.sub (:local.set pos)
                 (:local.get buf) (:local.get pos) (:i32.const 45) (:array.set :string)))
  '(:local.get size) '(:local.get pos) :i32.sub '(:array.new_default :string) '(:local.tee s)
  '(:i32.const 0) '(:local.get buf) '(:local.get pos) '(:local.get size) '(:local.get pos) :i32.sub
  '(:array.copy :string :string)
  '(:local.get s) :ref.as_non_null)
(defprim clysm-sys::%int-decimal-string (x) :value `(,@x (:call :int-decimal)))

;;; The decimal digits of the magnitude of an integer written at the end of
;;; the simple string BUF (a scratch buffer): the index of the first, or -1
;;; when they may not fit.  Within 64 bits: divisions of an i64 by 10;
;;; beyond: a copy of the magnitude divided in place by 10^9, as above.
(defhelper :int-decimal-into :ee-e ((buf (:ref :null :string)) (b (:ref :null :bignum))
                                    (m (:ref :null :limbs)) (src (:ref :null :limbs))
                                    (n :i32) (pos :i32) (i :i32) (k :i32) (r :i32) (small :i32)
                                    (v :i64) (q :i64) (rem :i64) (cur :i64))
  '(:local.get 1) '(:ref.cast :string) '(:local.tee buf) :array.len '(:local.set pos)
  '(:local.get 0) '(:ref.test :i31)
  '(:if nil nil
    ((:local.get 0) (:ref.cast :i31) :i31.get_s :i64.extend_i32_s (:local.set v)
     (:i32.const 1) (:local.set small))
    ((:local.get 0) (:ref.cast :bignum) (:local.tee b) (:struct.get :bignum 1) :ref.is_null
     (:if nil nil ((:local.get b) (:struct.get :bignum 2) (:local.set v)
                   (:i32.const 1) (:local.set small)))))
  ;; within 64 bits: V, its magnitude as an unsigned i64
  '(:local.get small)
  '(:if nil nil
    ((:local.get v) (:i64.const 0) :i64.lt_s
     (:if nil nil ((:i64.const 0) (:local.get v) :i64.sub (:local.set v)))
     (:loop again nil
       (:local.get pos) (:i32.const 1) :i32.sub (:local.set pos)
       (:local.get v) (:i64.const 10) :i64.div_u (:local.set q)
       (:local.get buf) (:local.get pos)
       (:local.get v) (:local.get q) (:i64.const 10) :i64.mul :i64.sub :i32.wrap_i64 (:i32.const 48) :i32.add
       (:array.set :string)
       (:local.get q) (:local.tee v) :i64.eqz :i32.eqz (:br_if again))
     (:local.get pos) :ref.i31 :return))
  ;; limbs
  '(:local.get b) '(:struct.get :bignum 1) '(:ref.cast! :limbs) '(:local.tee src) '(:call :mag-len) '(:local.set n)
  '(:local.get n) '(:i32.const 10) :i32.mul '(:local.get pos) :i32.gt_u
  '(:if nil nil ((:i32.const -1) :ref.i31 :return))
  '(:local.get n) '(:array.new_default :limbs) '(:local.set m)
  '(:local.get m) '(:i32.const 0) '(:local.get src) '(:i32.const 0) '(:local.get n) '(:array.copy :limbs :limbs)
  `(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       ;; m := m / 10^9, rem := the remainder
       (:i64.const 0) (:local.set rem) (:local.get n) (:local.set i)
       (:block ddone nil
         (:loop dagain nil
           (:local.get i) :i32.eqz (:br_if ddone)
           (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
           (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
           (:local.get m) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.tee cur)
           (:i64.const 1000000000) :i64.div_u (:local.set q)
           (:local.get m) (:local.get i) (:local.get q) :i32.wrap_i64 (:array.set :limbs)
           (:local.get cur) (:local.get q) (:i64.const 1000000000) :i64.mul :i64.sub (:local.set rem)
           (:br dagain)))
       (:local.get m) (:local.get n) (:i32.const 1) :i32.sub (:array.get :limbs) :i32.eqz
       (:if nil nil ((:local.get n) (:i32.const 1) :i32.sub (:local.set n)))
       ;; nine digits (the last chunk: without leading zeros)
       (:local.get rem) :i32.wrap_i64 (:local.set r) (:i32.const 0) (:local.set k)
       (:block wdone nil
         (:loop wagain nil
           (:local.get k) (:i32.const 9) :i32.ge_u (:br_if wdone)
           (:local.get n) :i32.eqz (:local.get r) :i32.eqz :i32.and (:br_if wdone)
           (:local.get pos) (:i32.const 1) :i32.sub (:local.set pos)
           (:local.get buf) (:local.get pos) (:local.get r) (:i32.const 10) :i32.rem_u (:i32.const 48) :i32.add
           (:array.set :string)
           (:local.get r) (:i32.const 10) :i32.div_u (:local.set r)
           (:local.get k) (:i32.const 1) :i32.add (:local.set k)
           (:br wagain)))
       (:br again)))
  '(:local.get pos) :ref.i31)
(defprim clysm-sys::%integer-decimal-into (x buf) :value `(,@x ,@buf (:call :int-decimal-into)))

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

;;; (u1, u0) / d for u1 < d (unsigned i64 locals U1 U0 D): two steps of 32 bits
;;; (Knuth D with normalization).  Leaves the quotient in Q1 * 2^32 + Q0 and
;;; the remainder in U21 >> S.  Uses the i64 locals S DH DL Q1 Q0 RHAT U21.
(defun udiv128-code ()
  `((:local.get d) :i64.clz (:local.tee s) :i64.eqz :i32.eqz
    (:if nil nil
         ((:local.get d) (:local.get s) :i64.shl (:local.set d)
          (:local.get u1) (:local.get s) :i64.shl (:local.get u0) (:i64.const 64) (:local.get s) :i64.sub :i64.shr_u
          :i64.or (:local.set u1)
          (:local.get u0) (:local.get s) :i64.shl (:local.set u0)))
    (:local.get d) (:i64.const 32) :i64.shr_u (:local.set dh)
    (:local.get d) (:i64.const #xFFFFFFFF) :i64.and (:local.set dl)
    ,@(loop for (q hi lo-digit) in `((q1 ((:local.get u1)) ((:local.get u0) (:i64.const 32) :i64.shr_u))
                                     (q0 ((:local.get u21)) ((:local.get u0) (:i64.const #xFFFFFFFF) :i64.and)))
            append `(,@hi (:local.get dh) :i64.div_u (:local.set ,q)
                     ,@hi (:local.get dh) :i64.rem_u (:local.set rhat)
                     (:block qdone nil
                       (:loop qagain nil
                         (:block dec nil
                           (:local.get ,q) (:i64.const 32) :i64.shr_u :i64.eqz :i32.eqz (:br_if dec)
                           (:local.get ,q) (:local.get dl) :i64.mul
                           (:local.get rhat) (:i64.const 32) :i64.shl ,@lo-digit :i64.or :i64.gt_u (:br_if dec)
                           (:br qdone))
                         (:local.get ,q) (:i64.const 1) :i64.sub (:local.set ,q)
                         (:local.get rhat) (:local.get dh) :i64.add (:local.tee rhat)
                         (:i64.const 32) :i64.shr_u :i64.eqz (:br_if qagain)))
                     ,@hi (:i64.const 32) :i64.shl ,@lo-digit :i64.or (:local.get ,q) (:local.get d) :i64.mul :i64.sub
                     (:local.set u21)))))

;;; (MOD (* a b) c) / (REM (* a b) c) of integers within 64 bits: the 128-bit
;;; product is reduced without making a bignum.  The fourth parameter is 1
;;; for MOD.  Other arguments: the generic product and division.
(defhelper :mulmod :eeei-e ((v :i32) (xa :i64) (xb :i64) (xc :i64) (ka :i32)
                            (a :i64) (b :i64) (p00 :i64) (mid :i64) (mid2 :i64)
                            (u1 :i64) (u0 :i64) (d :i64) (s :i64) (dh :i64) (dl :i64) (q1 :i64) (q0 :i64)
                            (rhat :i64) (u21 :i64) (r :i64))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     ,@(num-decode-code 1 'xb nil 'ka 'slow nil)
     ,@(num-decode-code 2 'xc nil 'ka 'slow nil)
     (:local.get xc) :i64.eqz (:br_if slow)
     (:block reduced nil
       ;; both factors within 32 bits: the product fits
       (:local.get xa) (:i64.const ,(expt 2 31)) :i64.add
       (:local.get xb) (:i64.const ,(expt 2 31)) :i64.add :i64.or (:i64.const 32) :i64.shr_u :i64.eqz
       (:if nil nil ((:local.get xa) (:local.get xb) :i64.mul (:local.get xc) :i64.rem_s (:local.set r) (:br reduced)))
       (:local.get xa) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if slow)
       (:local.get xb) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if slow)
       (:local.get xa) (:i64.const 0) :i64.lt_s (:if nil :i64 ((:i64.const 0) (:local.get xa) :i64.sub) ((:local.get xa)))
       (:local.set a)
       (:local.get xb) (:i64.const 0) :i64.lt_s (:if nil :i64 ((:i64.const 0) (:local.get xb) :i64.sub) ((:local.get xb)))
       (:local.set b)
       ;; |a| * |b| as (u1, u0)
       (:local.get a) (:i64.const #xFFFFFFFF) :i64.and (:local.get b) (:i64.const #xFFFFFFFF) :i64.and :i64.mul
       (:local.set p00)
       (:local.get a) (:i64.const #xFFFFFFFF) :i64.and (:local.get b) (:i64.const 32) :i64.shr_u :i64.mul
       (:local.get p00) (:i64.const 32) :i64.shr_u :i64.add (:local.set mid)
       (:local.get a) (:i64.const 32) :i64.shr_u (:local.get b) (:i64.const #xFFFFFFFF) :i64.and :i64.mul
       (:local.get mid) (:i64.const #xFFFFFFFF) :i64.and :i64.add (:local.set mid2)
       (:local.get mid2) (:i64.const 32) :i64.shl (:local.get p00) (:i64.const #xFFFFFFFF) :i64.and :i64.or
       (:local.set u0)
       (:local.get a) (:i64.const 32) :i64.shr_u (:local.get b) (:i64.const 32) :i64.shr_u :i64.mul
       (:local.get mid) (:i64.const 32) :i64.shr_u :i64.add (:local.get mid2) (:i64.const 32) :i64.shr_u :i64.add
       (:local.set u1)
       (:local.get xc) (:i64.const 0) :i64.lt_s (:if nil :i64 ((:i64.const 0) (:local.get xc) :i64.sub) ((:local.get xc)))
       (:local.set d)
       ;; the high word below the divisor, then 128 / 64
       (:local.get u1) (:local.get d) :i64.rem_u (:local.set u1)
       ,@(udiv128-code)
       (:local.get u21) (:local.get s) :i64.shr_u (:local.set r)
       ;; the remainder of a truncation has the sign of the product
       (:local.get xa) (:local.get xb) :i64.xor (:i64.const 0) :i64.lt_s
       (:if nil nil ((:i64.const 0) (:local.get r) :i64.sub (:local.set r))))
     ;; MOD: a remainder of the other sign than the divisor moves by it
     (:local.get 3)
     (:if nil nil
          ((:local.get r) :i64.eqz :i32.eqz
           (:local.get r) (:local.get xc) :i64.xor (:i64.const 0) :i64.lt_s :i32.and
           (:if nil nil ((:local.get r) (:local.get xc) :i64.add (:local.set r)))))
     (:local.get r) (:call :i64-int) :return)
  '(:local.get 3)
  '(:if nil :eqref
        ((:local.get 0) (:local.get 1) (:call :mul) (:local.get 2) (:call :mod))
        ((:local.get 0) (:local.get 1) (:call :mul) (:local.get 2) (:call :rem))))

;;; A bignum of up to 127 bits divided by an integer within 64 bits, when the
;;; quotient is within 64 bits too (the product of two such integers modulo
;;; a third, for one): MODE as for :divmod; the quotient, the remainder as the
;;; second value.  Null when the arguments are not of that kind.
(defhelper :wide-divmod :eei-e ((v :i32) (xb :i64) (ka :i32) (m (:ref :null :limbs)) (n :i32)
                                (u1 :i64) (u0 :i64) (d :i64) (s :i64) (dh :i64) (dl :i64) (q1 :i64) (q0 :i64)
                                (rhat :i64) (u21 :i64) (sx :i64) (w :i64) (wr :i64) (wt :i64) (wt2 :i64))
  `(:block fail nil
     (:local.get 0) (:ref.test :bignum) :i32.eqz (:br_if fail)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) :i64.eqz :i32.eqz (:br_if fail)
     ,@(num-decode-code 1 'xb nil 'ka 'fail nil)
     (:local.get xb) :i64.eqz (:br_if fail)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) (:local.set m)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) (:call :mag-len) (:local.tee n)
     (:i32.const 4) :i32.gt_u (:br_if fail)
     (:local.get n) (:i32.const 2) :i32.lt_u (:br_if fail)
     ;; the magnitude (below 2^127) as (u1, u0)
     (:local.get m) (:i32.const 0) (:array.get :limbs) :i64.extend_i32_u
     (:local.get m) (:i32.const 1) (:array.get :limbs) :i64.extend_i32_u (:i64.const 32) :i64.shl :i64.or
     (:local.set u0)
     (:local.get n) (:i32.const 2) :i32.gt_u
     (:if nil nil ((:local.get m) (:i32.const 2) (:array.get :limbs) :i64.extend_i32_u (:local.set u1)))
     (:local.get n) (:i32.const 3) :i32.gt_u
     (:if nil nil ((:local.get m) (:i32.const 3) (:array.get :limbs) (:i32.const 0) :i32.lt_s (:br_if fail)
                   (:local.get m) (:i32.const 3) (:array.get :limbs) :i64.extend_i32_u (:i64.const 32) :i64.shl
                   (:local.get u1) :i64.or (:local.set u1)))
     ;; |divisor|; the quotient must fit in 64 bits
     (:local.get xb) (:i64.const 0) :i64.lt_s
     (:if nil :i64 ((:i64.const 0) (:local.get xb) :i64.sub) ((:local.get xb))) (:local.set d)
     (:local.get u1) (:local.get d) :i64.ge_u (:br_if fail)
     ,@(udiv128-code)
     ;; quotient and remainder magnitudes, then the signs of a truncation
     (:local.get q1) (:i64.const 32) :i64.shl (:local.get q0) :i64.or (:local.tee w)
     ;; (below 2^62: the adjusted quotient fits)
     (:i64.const 62) :i64.shr_u :i64.eqz :i32.eqz (:br_if fail)
     (:local.get u21) (:local.get s) :i64.shr_u (:local.set wr)
     (:i64.const 1) (:local.set sx)
     (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 0)
     (:if nil nil ((:i64.const -1) (:local.set sx) (:i64.const 0) (:local.get wr) :i64.sub (:local.set wr)))
     (:local.get sx) (:local.get xb) :i64.xor (:i64.const 0) :i64.lt_s
     (:if nil nil ((:i64.const 0) (:local.get w) :i64.sub (:local.set w)))
     ,@(loop for mode from 1 to 3
             collect `(:local.get 2) collect `(:i32.const ,mode) collect :i32.eq
             collect `(:if nil nil ,(int-divmod-adjust-code :i64 mode 'sx 'xb 'w 'wr 'wt 'wt2)))
     (:global.get :mv-buf) (:i32.const 0) (:local.get wr) (:call :i64-int) (:array.set :vector)
     (:i32.const 2) (:global.set :mv-count)
     (:local.get w) (:call :i64-int) :return)
  '(:ref.null :eq))

;;; MOD / REM fast paths: fixnums, then integers within 64 bits
(defun divop-front-body (gen modp)
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
    (:local.get 0) (:local.get 1) (:return_call ,gen)))

(defun divop-helper-body (lisp-fn modp)
  `((:block slow nil
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
    (:local.get 0) (:local.get 1) (:i32.const ,(if modp 1 0)) (:call :wide-divmod) :ref.is_null :i32.eqz
    (:if nil nil ((:global.get :mv-buf) (:i32.const 0) (:array.get :vector) :return))
    (:local.get 0) (:local.get 1)
    (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))))

(defhelper :mod-gen :ee-e ((v :i32) (xa :i64) (xb :i64) (ka :i32) (w :i64))
  (cons :progn-marker (divop-helper-body 'clysm-sys::%mod-slow t)))
(defhelper :rem-gen :ee-e ((v :i32) (xa :i64) (xb :i64) (ka :i32) (w :i64))
  (cons :progn-marker (divop-helper-body 'clysm-sys::%rem-slow nil)))
(defhelper :mod :ee-e ((x :i32) (y :i32) (r :i32))
  (cons :progn-marker (divop-front-body :mod-gen t)))
(defhelper :rem :ee-e ((x :i32) (y :i32) (r :i32))
  (cons :progn-marker (divop-front-body :rem-gen nil)))
(defprim mod (a b) :value `(,@a ,@b (:call :mod)))
(defprim rem (a b) :value `(,@a ,@b (:call :rem)))

(defprim clysm-sys::%backtrace (fd) :value `(,@(idx-code fd) (:call :host-backtrace) ,@(nil-code)))
(defprim clysm-sys::%is-tty () :value `((:call :host-is-tty) :ref.i31))

;; instantiate a module compiled in the image and run it (see src/live.lisp)
;; (RUN is called from WebAssembly, without a JavaScript frame in between,
;; so that the code of the module can wait: JS:AWAIT, threads)
(defprim clysm-sys::%compile-module (bytes consts) :mv
  (checked-call
   `(,@(ext-code consts) ,@(ext-code bytes) (:call :host-instantiate-module)
     (:call_ref :x-e))))
(defprim clysm-sys::%flush () :value `((:call :host-flush) ,@(nil-code)))

;;; ------------------------------------------------------------
;;; checked casts: ref.cast to these types signals TYPE-ERROR on failure
;;; ------------------------------------------------------------

(defparameter *checked-cast-types*
  '((:i31 . fixnum) (:cons . cons) (:symbol . symbol) (:string . simple-string)
    (:vector . simple-vector) (:instance . structure-object) (:mdarray . array)
    (:double . double-float) (:single . single-float) (:bignum . integer) (:ratio . ratio)
    (:complex . complex) (:complex-d . (complex double-float)) (:complex-s . (complex single-float))
    (:package . package) (:bytes . (simple-array (unsigned-byte 8) (*)))
    (:bits . simple-bit-vector) (:f64vec . (simple-array double-float (*))) (:foreign . t)
    (:limbs . t) (:closure . function)))

(defun define-checked-casts (&key lazy)
  "Define the functions that checked casts call (CAST-REWRITES).  With LAZY,
they are not added: returns a table of name -> function adding one, for
those the code calls (modules made at run time call few of them)."
  (let ((pending (and lazy (make-hash-table :test 'equal))))
  (dolist (e *checked-cast-types* pending)
    (let* ((type (car e)) (lisp-type (cdr e))
           (fname (list :castfn type))
           (tname (list :cast-type type)))
      (unless (gethash tname *extra-types*)
        (setf (gethash tname *extra-types*) t)
        (add-type *m* tname `(:func (:eqref) ((:ref ,type))))
        ;; the block type of the inline cast (wasm.lisp)
        (add-type *m* (list :cast-bt type) `(:func (:eqref) ((:ref ,type)))))
      (flet ((add ()
               (add-func *m* fname tname
                         :body `((:block fail :eqref
                                   (:local.get 0) (:br_on_cast_fail fail :eqref (:ref ,type)) :return)
                                 :drop
                                 (:local.get 0) ,@(const-init-code lisp-type)
                                 (:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2))
                                 (:ref.cast ,type)))))
        (if lazy (setf (gethash fname pending) #'add) (add)))
      (push (cons type fname) (wasm-module-cast-rewrites *m*))))))

;;; ------------------------------------------------------------
;;; Heap images (runtime/image.lisp)
;;; ------------------------------------------------------------
;;; An image of the heap is a sequence of 32-bit words in the linear memory.
;;; A reference is one word: an i31 (fixnum, character) is its value * 2 + 1,
;;; NIL... are objects: 0 is null, and otherwise (index + 1) * 2 indexes the
;;; objects (:IMG-OBJS), the image's own (the objects of its immutable
;;; globals, see ADD-IMAGE-FUNCTIONS) then those of the heap image.  After a
;;; header (magic, version, number of image objects, of heap objects, of
;;; records) come the heap objects, each a kind (+IMG-KINDS+) and its
;;; immutable contents, made in that order (an object refers only to objects
;;; before it in its immutable fields); then the records that set the
;;; mutable fields of objects (the object, then its fields, by its type);
;;; then the roots (a simple vector).

(defconstant +img-magic+ #x494D4C43)    ; "CLMI"
(defconstant +img-version+ 5)
(defparameter +img-kinds+
  '(:cons :vector :string8 :string32 :limbs :cell :instance :package :symbol :closure
    :mdarray :double :single :bignum :ratio :complex :complex-d :complex-s :storage :fsbuf :sinst))
(defun img-kind (k) (position k +img-kinds+))

;; saving: the table index of entry I of the closure F (0-5: the entries for
;; 0-4 and N arguments, 6: the function taking keyword arguments), or -1
(defhelper :closure-code :ei-e ((c (:ref :null :closure)) (f (:ref :null :func)))
  '(:local.get 0) '(:ref.cast :closure) '(:local.set c)
  (loop for i from 0 to 5
        append `((:local.get 1) (:i32.const ,i) :i32.eq
                 (:if nil nil ((:local.get c) ,@(clo-field-code i) (:call :host-code-index)
                               :ref.i31 :return))))
  `((:local.get 1) (:i32.const 7) :i32.eq
    (:if nil nil ((:local.get c) ,@(clo-field-code +clo-entry-a+) (:call :host-code-index) :ref.i31 :return))
    (:local.get c) ,@(clo-field-code +clo-kmain+) (:local.tee f) :ref.is_null
    (:if nil nil ((:i32.const -1) :ref.i31 :return))
    (:local.get f) (:call :host-code-index) :ref.i31))
(defprim clysm-sys::%closure-code (f i) :value `(,@f ,@(idx-code i) (:call :closure-code)))
;; the values of the free variables of a closure (heap images)
(defprim clysm-sys::%closure-env-length (f) :value `(,@f (:call :closure-env-length) :ref.i31))
(defprim clysm-sys::%closure-env-ref (f i) :value `(,@f ,@(idx-code i) (:call :closure-env-ref)))

(defun envclo-switch-code (fn &optional vector-code)
  "For each closure type with free variables in fields, (FN N TYPE) run when
the closure in local 0 is of that type; then VECTOR-CODE when it is an
:ENVCLOV."
  `(,@(loop for n from 1 to +clo-env-max+
            for type = (envclo-type n)
            append `((:local.get 0) (:ref.test ,type)
                     (:if nil nil ,(funcall fn n type))))
    ,@(when vector-code
        `((:local.get 0) (:ref.test :envclov) (:if nil nil ,vector-code)))))

(defhelper :closure-env-length :e-i ()
  (envclo-switch-code (lambda (n type) (declare (ignore type)) `((:i32.const ,n) :return))
                      `((:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-env+) :array.len :return))
  '(:i32.const 0))
(defhelper :closure-env-ref :ei-e ()
  (envclo-switch-code
   (lambda (n type)
     (img-switch-code '((:local.get 1))
                      (loop for i below n
                            collect `((:local.get 0) (:ref.cast! ,type) (:struct.get ,type ,(+ +clo-env+ i))
                                      :return))))
   `((:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-env+) (:local.get 1)
     (:array.get :vector) :return))
  '(:ref.null :eq))
(defhelper :closure-env-set :eie-e ()
  (envclo-switch-code
   (lambda (n type)
     (img-switch-code '((:local.get 1))
                      (loop for i below n
                            collect `((:local.get 0) (:ref.cast! ,type) (:local.get 2)
                                      (:struct.set ,type ,(+ +clo-env+ i))
                                      (:local.get 2) :return))))
   `((:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-env+) (:local.get 1)
     (:local.get 2) (:array.set :vector) (:local.get 2) :return))
  '(:local.get 2))
;; a closure like CLO (local 0) with the code CODE (local 1): the same name
;; and free variables
(defhelper :closure-with-code :ee-e ()
  (envclo-switch-code
   (lambda (n type)
     `((:local.get 1) (:ref.cast! :code)
       (:local.get 0) (:ref.cast! ,type) (:struct.get ,type ,+clo-name+)
       ,@(loop for i below n collect `(:local.get 0) collect `(:ref.cast! ,type)
               collect `(:struct.get ,type ,(+ +clo-env+ i)))
       (:struct.new ,type) :return))
   `((:local.get 1) (:ref.cast! :code)
     (:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-name+)
     (:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-env+)
     (:struct.new :envclov) :return))
  '(:local.get 1) '(:ref.cast! :code)
  '(:local.get 0) '(:ref.cast! :closure) `(:struct.get :closure ,+clo-name+)
  '(:struct.new :closure))
;; a closure of the code CODE (local 0) with N (local 1) free variables, all
;; null (heap images: they are set afterwards)
(defhelper :make-env-closure :ei-e ()
  (loop for n from 1 to +clo-env-max+
        append `((:local.get 1) (:i32.const ,n) :i32.eq
                 (:if nil nil ((:local.get 0) (:ref.cast! :code) (:ref.null :eq)
                               ,@(loop repeat n collect '(:ref.null :eq))
                               (:struct.new ,(envclo-type n)) :return))))
  '(:local.get 1) :i32.eqz
  '(:if nil nil ((:local.get 0) (:ref.cast! :code) (:ref.null :eq) (:struct.new :closure) :return))
  '(:local.get 0) '(:ref.cast! :code) '(:ref.null :eq) '(:local.get 1) '(:array.new_default :vector)
  '(:struct.new :envclov))
(defprim clysm-sys::%closure-raw-klayout (f) :value
  `(,@f (:ref.cast :closure) ,@(clo-field-code +clo-klayout+)))
(defprim clysm-sys::%symbol-raw-value (s) :value `(,@s (:ref.cast :symbol) (:struct.get :symbol ,+sym-value+)))
(defprim clysm-sys::%symbol-raw-plist (s) :value `(,@s (:ref.cast :symbol) (:struct.get :symbol ,+sym-plist+)))
(defprim clysm-sys::%symbol-raw-function (s) :value
  `(,@s (:ref.cast :symbol) (:struct.get :symbol ,+sym-function+)))
(defprim clysm-sys::%instance-raw-hash (x) :value `(,@x (:ref.cast :instance) (:struct.get :instance 2) :ref.i31))
(defprim clysm-sys::%limbs-p (x) :test `(,@x (:ref.test :limbs)))
(defprim clysm-sys::%bignum-raw-limbs (x) :value `(,@x (:ref.cast :bignum) (:struct.get :bignum 1)))
(defprim clysm-sys::%bignum-raw-sign (x) :value `(,@x (:ref.cast :bignum) (:struct.get :bignum 0) :ref.i31))
(defprim clysm-sys::%mdarray-field (a i) :value
  ;; the raw field I (0-7) of the array header A (5, 6 and 7 are integers)
  (let ((m (new-local '(:ref :null :mdarray))) (k (new-local :i32)))
    (labels ((field (f)
               (if (= f 7)
                   `((:local.get ,m) (:struct.get :mdarray 7) :ref.i31)
                   `((:local.get ,k) (:i32.const ,f) :i32.eq
                     (:if nil :eqref ((:local.get ,m) (:struct.get :mdarray ,f) ,@(if (>= f 5) '(:ref.i31)))
                          ,(field (1+ f)))))))
      `(,@a (:ref.cast :mdarray) (:local.set ,m) ,@(idx-code i) (:local.set ,k) ,@(field 0)))))
(defprim clysm-sys::%image-objects () :value '((:call :image-objects)))
(defprim clysm-sys::%image-mutables () :value '((:call :image-mutables)))
(defprim clysm-sys::%module-count () :value '((:call :host-module-count) :ref.i31))
(defprim clysm-sys::%module-snap (i) :value `(,@(idx-code i) (:call :host-module-snap)))
(defprim clysm-sys::%symbol-raw-package (s) :value
  `(,@s (:ref.cast :symbol) (:struct.get :symbol ,+sym-package+)))
(defprim clysm-sys::%fsbuf-raw-octets (b) :value `(,@b (:ref.cast :fsbuf) (:struct.get :fsbuf 0)))
;; words that do not fit a fixnum, written into the (array i32) BUF at POS
(defprim clysm-sys::%img-put-i31 (buf pos x) :value
  `(,@buf (:ref.cast :limbs) ,@(idx-code pos) ,@x (:ref.cast! :i31) :i31.get_s
    (:i32.const 1) :i32.shl (:i32.const 1) :i32.or (:array.set :limbs) ,@(nil-code)))
(defprim clysm-sys::%img-put-word (buf pos w) :value
  `(,@buf (:ref.cast :limbs) ,@(idx-code pos) ,@(idx-code w) (:array.set :limbs) ,@(nil-code)))
(defun img-put-i64-code (buf pos value-code)
  (let ((b (new-local '(:ref :null :limbs))) (p (new-local :i32)) (v (new-local :i64)))
    `(,@buf (:ref.cast :limbs) (:local.set ,b) ,@(idx-code pos) (:local.set ,p)
      ,@value-code (:local.set ,v)
      (:local.get ,b) (:local.get ,p) (:local.get ,v) :i32.wrap_i64 (:array.set :limbs)
      (:local.get ,b) (:local.get ,p) (:i32.const 1) :i32.add
      (:local.get ,v) (:i64.const 32) :i64.shr_u :i32.wrap_i64 (:array.set :limbs)
      ,@(nil-code))))
(defprim clysm-sys::%img-put-double (buf pos x) :value
  (img-put-i64-code buf pos `(,@x (:ref.cast :double) (:struct.get :double 0) :i64.reinterpret_f64)))
(defprim clysm-sys::%img-put-bignum-value (buf pos x) :value
  (img-put-i64-code buf pos `(,@x (:ref.cast :bignum) (:struct.get :bignum 2))))
(defprim clysm-sys::%img-put-single (buf pos x) :value
  `(,@buf (:ref.cast :limbs) ,@(idx-code pos) ,@x (:ref.cast :single) (:struct.get :single 0)
    :i32.reinterpret_f32 (:array.set :limbs) ,@(nil-code)))
;; the word HI * 65536 + LO
(defprim clysm-sys::%img-put-halves (buf pos hi lo) :value
  `(,@buf (:ref.cast :limbs) ,@(idx-code pos) ,@(idx-code hi) (:i32.const 16) :i32.shl ,@(idx-code lo) :i32.or
    (:array.set :limbs) ,@(nil-code)))
;; the elements of the (array i32) SRC (limbs) at POS
(defprim clysm-sys::%img-put-limbs (buf pos src) :value
  `(,@buf (:ref.cast :limbs) ,@(idx-code pos) ,@src (:ref.cast :limbs) (:i32.const 0)
    ,@src (:ref.cast :limbs) :array.len (:array.copy :limbs :limbs) ,@(nil-code)))
;; the first N words of BUF into the linear memory from 0 (for the host)
(defprim clysm-sys::%img-to-memory (buf n) :value
  (let ((b (new-local '(:ref :null :limbs))) (len (new-local :i32)) (k (new-local :i32))
        (done (gensym "DONE")) (again (gensym "AGAIN")))
    `(,@buf (:ref.cast :limbs) (:local.set ,b) ,@(idx-code n) (:local.set ,len)
      (:local.get ,len) (:i32.const 2) :i32.shl (:local.set ,k)
      ,@(mem-ensure-code k)
      (:i32.const 0) (:local.set ,k)
      (:block ,done nil
        (:loop ,again nil
          (:local.get ,k) (:local.get ,len) :i32.ge_u (:br_if ,done)
          (:local.get ,k) (:i32.const 2) :i32.shl
          (:local.get ,b) (:local.get ,k) (:array.get :limbs) (:i32.store)
          (:local.get ,k) (:i32.const 1) :i32.add (:local.set ,k)
          (:br ,again)))
      ,@(nil-code))))

;; loading
(defhelper :img-word :v-i ((w :i32))
  '(:global.get :img-pos) '(:i32.load) '(:local.set w)
  '(:global.get :img-pos) '(:i32.const 4) :i32.add '(:global.set :img-pos)
  '(:local.get w))
(defhelper :img-ref :v-e ((w :i32))
  '(:call :img-word) '(:local.tee w) '(:i32.const 1) :i32.and
  '(:if nil nil ((:local.get w) (:i32.const 1) :i32.shr_s :ref.i31 :return))
  '(:local.get w) :i32.eqz
  '(:if nil nil ((:ref.null :eq) :return))
  '(:global.get :img-objs) '(:local.get w) '(:i32.const 1) :i32.shr_u '(:i32.const 1) :i32.sub
  '(:array.get :vector))
(defhelper :img-i64 :v-l ((lo :i32))
  '(:call :img-word) '(:local.set lo)
  '(:call :img-word) :i64.extend_i32_u '(:i64.const 32) :i64.shl
  '(:local.get lo) :i64.extend_i32_u :i64.or)

(defun img-switch-code (index-code cases)
  "Instructions running the code of the case INDEX-CODE (an i32) selects in
CASES, a list of instruction lists (each leaves nothing), or trapping."
  (let ((labels (loop for nil in cases collect (gensym "CASE")))
        (end (gensym "END")) (bad (gensym "BAD")))
    (labels ((nest (ls cs)
               ;; the innermost block holds the br_table; after the end of
               ;; the block of case I comes its code
               (if (null ls)
                   `((:block ,bad nil
                       ,@index-code (:br_table ,@labels ,bad))
                     :unreachable)
                   `((:block ,(car (last ls)) nil
                       ,@(nest (butlast ls) (butlast cs)))
                     ,@(car (last cs))
                     (:br ,end)))))
      `((:block ,end nil ,@(nest labels cases))))))

(defun img-loop-code (counter limit body)
  "Instructions running BODY while the i32 local COUNTER is below the i32
local LIMIT, incrementing it."
  (let ((done (gensym "DONE")) (again (gensym "AGAIN")))
    `((:block ,done nil
        (:loop ,again nil
          (:local.get ,counter) (:local.get ,limit) :i32.ge_u (:br_if ,done)
          ,@body
          (:local.get ,counter) (:i32.const 1) :i32.add (:local.set ,counter)
          (:br ,again))))))

(defun img-code-ref (type)
  "Instructions: the function of the next word's index in the table, as TYPE."
  `((:call :img-word) (:table.get) (:ref.cast! ,type)))

(defun img-alloc-cases ()
  "The code making each kind of object (+IMG-KINDS+) into the local X."
  (flet ((done (&rest code) `(,@(apply #'append code) (:local.set x))))
    (loop for kind in +img-kinds+
          collect
          (ecase kind
            (:cons (done '((:struct.new_default :cons))))
            (:vector (done '((:call :img-word) (:array.new_default :vector))))
            (:string8
             `((:call :img-word) (:local.tee len) (:array.new_default :string) (:local.set s)
               (:i32.const 0) (:local.set j)
               ,@(img-loop-code 'j 'len
                                '((:local.get s) (:local.get j)
                                  (:global.get :img-pos) (:local.get j) :i32.add (:i32.load8_u)
                                  (:array.set :string)))
               (:global.get :img-pos) (:local.get len) (:i32.const 3) :i32.add (:i32.const -4) :i32.and
               :i32.add (:global.set :img-pos)
               (:local.get s) (:local.set x)))
            ((:string32 :limbs)
             (let ((type (if (eq kind :limbs) :limbs :string)))
               `((:call :img-word) (:local.tee len) (:array.new_default ,type) (:local.set x)
                 (:i32.const 0) (:local.set j)
                 ,@(img-loop-code 'j 'len
                                  `((:local.get x) (:ref.cast! ,type) (:local.get j) (:call :img-word)
                                    (:array.set ,type))))))
            (:cell (done '((:struct.new_default :cell))))
            (:instance (done '((:ref.null :eq) (:call :img-ref) (:ref.cast! :vector) (:call :img-word)
                               (:struct.new :instance))))
            (:package (done '((:struct.new_default :package))))
            (:symbol (done '((:call :img-ref) (:ref.cast! :string) (:ref.null :package) (:ref.null :eq)
                             (:ref.null :closure) (:ref.null :eq) (:call :img-word) (:i32.const 0)
                             (:struct.new :symbol))))
            (:closure
             ;; the words: the 6 entries, the function taking keywords (-1:
             ;; none), the layout of its keywords, the entry taking a list, the number of free
             ;; variables (their values are mutable fields)
             (done (loop for i from 0 to 4 append (img-code-ref (fixed-entry-type i)))
                   (img-code-ref :fN)
                   '((:call :img-word) (:local.tee w) (:i32.const -1) :i32.eq
                     (:if nil (:ref :null :func) ((:ref.null :func)) ((:local.get w) (:table.get)))
                     (:call :img-ref)
                     (:call :img-word) (:table.get) (:ref.cast! :fA)
                     (:struct.new :code)
                     (:call :img-word) (:call :make-env-closure))))
            (:mdarray (done '((:struct.new_default :mdarray))))
            (:double (done '((:call :img-i64) :f64.reinterpret_i64 (:struct.new :double))))
            (:single (done '((:call :img-word) :f32.reinterpret_i32 (:struct.new :single))))
            (:bignum (done '((:call :img-word) (:call :img-ref) (:ref.cast :null :limbs) (:call :img-i64)
                             (:struct.new :bignum))))
            (:ratio (done '((:call :img-ref) (:call :img-ref) (:struct.new :ratio))))
            (:complex (done '((:call :img-ref) (:call :img-ref) (:struct.new :complex))))
            (:complex-d (done '((:call :img-i64) :f64.reinterpret_i64 (:call :img-i64) :f64.reinterpret_i64
                                (:struct.new :complex-d))))
            (:complex-s (done '((:call :img-word) :f32.reinterpret_i32 (:call :img-word) :f32.reinterpret_i32
                                (:struct.new :complex-s))))
            (:storage
             `((:call :img-word) (:call :img-word) (:local.tee len) (:call :make-storage) (:local.set x)
               (:i32.const 0) (:local.set j)
               ,@(img-loop-code 'j 'len '((:local.get x) (:local.get j) (:call :img-ref) (:call :typed-set) :drop))))
            (:fsbuf (done '((:call :img-ref) (:ref.cast :null :bytes) (:call :img-word) (:call :img-word)
                            (:struct.new :fsbuf))))
            ;; a structure whose slots are fields: their number, then its hash
            (:sinst '((:call :img-word) (:call :sinst-make) (:local.tee x) (:ref.cast! :instance)
                      (:call :img-word) (:struct.set :instance 2)))))))

(defun img-fill-code ()
  "The code setting the mutable fields of the object in the local X from the
words that follow, by its type."
  (let ((end (gensym "END")))
    (flet ((fields (type n &optional cast)
             (loop for f below n
                   append `((:local.get x) (:ref.cast! ,type) (:call :img-ref)
                            ,@(let ((c (if (functionp cast) (funcall cast f) cast)))
                                (and c `((:ref.cast :null ,c))))
                            (:struct.set ,type ,f)))))
      `((:block ,end nil
          (:local.get x) (:ref.test :cons)
          (:if nil nil (,@(fields :cons 2) (:br ,end)))
          (:local.get x) (:ref.test :vector)
          (:if nil nil ((:i32.const 0) (:local.set j)
                        (:local.get x) (:ref.cast! :vector) :array.len (:local.set len)
                        ,@(img-loop-code 'j 'len '((:local.get x) (:ref.cast! :vector) (:local.get j)
                                                   (:call :img-ref) (:array.set :vector)))
                        (:br ,end)))
          (:local.get x) (:ref.test :symbol)
          (:if nil nil ((:local.get x) (:ref.cast! :symbol) (:call :img-ref) (:ref.cast :null :package)
                        (:struct.set :symbol ,+sym-package+)
                        (:local.get x) (:ref.cast! :symbol) (:call :img-ref) (:struct.set :symbol ,+sym-value+)
                        (:local.get x) (:ref.cast! :symbol) (:call :img-ref) (:ref.cast :null :closure)
                        (:struct.set :symbol ,+sym-function+)
                        (:local.get x) (:ref.cast! :symbol) (:call :img-ref) (:struct.set :symbol ,+sym-plist+)
                        (:local.get x) (:ref.cast! :symbol) (:call :img-word) (:struct.set :symbol ,+sym-flags+)
                        (:br ,end)))
          (:local.get x) (:ref.test :closure)
          (:if nil nil ((:local.get x) (:ref.cast! :closure) (:call :img-ref) (:struct.set :closure ,+clo-name+)
                        (:i32.const 0) (:local.set j)
                        (:local.get x) (:call :closure-env-length) (:local.set len)
                        ,@(img-loop-code 'j 'len '((:local.get x) (:local.get j) (:call :img-ref)
                                                   (:call :closure-env-set) :drop))
                        (:br ,end)))
          (:local.get x) (:ref.test :si1)
          (:if nil nil ((:local.get x) (:ref.cast! :instance) (:call :img-ref) (:struct.set :instance 0)
                        (:i32.const 0) (:local.set j)
                        (:local.get x) (:call :sinst-size) (:local.set len)
                        ,@(img-loop-code 'j 'len '((:local.get x) (:local.get j) (:call :img-ref)
                                                   (:call :sinst-set) :drop))
                        (:br ,end)))
          (:local.get x) (:ref.test :instance)
          (:if nil nil ((:local.get x) (:ref.cast! :instance) (:call :img-ref) (:struct.set :instance 0)
                        (:local.get x) (:ref.cast! :instance) (:call :img-ref) (:ref.cast! :vector)
                        (:struct.set :instance 1)
                        (:br ,end)))
          (:local.get x) (:ref.test :cell)
          (:if nil nil (,@(fields :cell 1) (:br ,end)))
          (:local.get x) (:ref.test :package)
          (:if nil nil (,@(fields :package 8) (:br ,end)))
          (:local.get x) (:ref.test :mdarray)
          (:if nil nil (,@(fields :mdarray 5)
                        (:local.get x) (:ref.cast! :mdarray) (:call :img-word) (:struct.set :mdarray 5)
                        (:local.get x) (:ref.cast! :mdarray) (:call :img-word) (:struct.set :mdarray 6)
                        (:local.get x) (:ref.cast! :mdarray) (:call :img-word) (:struct.set :mdarray 7)
                        (:br ,end)))
          :unreachable)))))

;; image_load(offset): make the objects of the heap image at OFFSET in the
;; linear memory (the host has instantiated the modules compiled in the image
;; first); the roots are then in :IMG-ROOTS
(defun img-load-code ()
  (list
  '(:local.get 0) '(:global.set :img-pos)
  `(:call :img-word) `(:i32.const ,+img-magic+) :i32.ne '(:if nil nil (:unreachable))
  `(:call :img-word) `(:i32.const ,+img-version+) :i32.ne '(:if nil nil (:unreachable))
  '(:call :img-word) '(:local.set nbase) '(:call :img-word) '(:local.set nheap)
  '(:call :img-word) '(:local.set nrec)
  '(:local.get nbase) '(:local.get nheap) :i32.add '(:array.new_default :vector) '(:global.set :img-objs)
  ;; the image's objects
  '(:call :image-objects) '(:ref.cast! :vector) '(:local.set chunks)
  '(:i32.const 0) '(:local.set k) '(:i32.const 0) '(:local.set j)
  '(:local.get chunks) :array.len '(:local.set end)
  (img-loop-code 'j 'end
                 '((:local.get chunks) (:local.get j) (:array.get :vector) (:ref.cast! :vector) (:local.set c)
                   (:global.get :img-objs) (:local.get k) (:local.get c) (:i32.const 0)
                   (:local.get c) :array.len (:array.copy :vector :vector)
                   (:local.get k) (:local.get c) :array.len :i32.add (:local.set k)))
  '(:local.get k) '(:local.get nbase) :i32.ne '(:if nil nil (:unreachable))
  ;; the heap objects
  '(:local.get nbase) '(:local.get nheap) :i32.add '(:local.set end)
  (img-loop-code 'k 'end
                 `(,@(img-switch-code '((:call :img-word)) (img-alloc-cases))
                   (:global.get :img-objs) (:local.get k) (:local.get x) (:array.set :vector)))
  ;; their mutable fields
  '(:i32.const 0) '(:local.set k)
  (img-loop-code 'k 'nrec `((:call :img-ref) (:local.set x) ,@(img-fill-code)))
  '(:call :img-ref) '(:global.set :img-roots)))

(defun img-inline (code)
  "CODE with the words and references read in place (the position in the
local POS) instead of by calls of :IMG-WORD, :IMG-REF and :IMG-I64: the
baseline compiler, which compiles IMAGE_LOAD, does not inline calls."
  (let ((word '((:local.get pos) (:i32.load) (:local.get pos) (:i32.const 4) :i32.add (:local.set pos))))
    (labels ((ref ()
               (let ((r (gensym "REF")))
                 `((:block ,r :eqref
                     ,@word (:local.tee rw) (:i32.const 1) :i32.and
                     (:if nil nil ((:local.get rw) (:i32.const 1) :i32.shr_s :ref.i31 (:br ,r)))
                     (:local.get rw) :i32.eqz
                     (:if nil nil ((:ref.null :eq) (:br ,r)))
                     (:local.get objs) (:local.get rw) (:i32.const 1) :i32.shr_u (:i32.const 1) :i32.sub
                     (:array.get :vector)))))
             (walk (list)
               (loop for x in list
                     append (cond ((equal x '(:call :img-word)) word)
                                  ((equal x '(:call :img-ref)) (ref))
                                  ((equal x '(:call :img-i64))
                                   `(,@word (:local.set rw) ,@word :i64.extend_i32_u (:i64.const 32) :i64.shl
                                     (:local.get rw) :i64.extend_i32_u :i64.or))
                                  ((equal x '(:global.get :img-pos)) '((:local.get pos)))
                                  ((equal x '(:global.set :img-pos)) '((:local.set pos)))
                                  ((equal x '(:global.get :img-objs)) '((:local.get objs)))
                                  ((and (consp x) (member (car x) '(:block :loop)))
                                   (list (list* (first x) (second x) (third x) (walk (cdddr x)))))
                                  ((and (consp x) (eq (car x) :if))
                                   (list (if (cddddr x)
                                             (list :if (second x) (third x) (walk (fourth x)) (walk (fifth x)))
                                             (list :if (second x) (third x) (walk (fourth x))))))
                                  (t (list x))))))
      (walk code))))

(defhelper :image-load :i-v ((nbase :i32) (nheap :i32) (nrec :i32) (k :i32) (end :i32)
                              (len :i32) (j :i32) (w :i32)
                              (s (:ref :null :string)) (x :eqref) (chunks (:ref :null :vector))
                              (c (:ref :null :vector)) (pos :i32) (rw :i32) (objs (:ref :null :vector)))
  (let ((code (loop for x in (img-load-code)
                    if (and (consp x) (consp (car x))) append x else collect x)))
    ;; (the objects in the local OBJS once the vector is made)
    (img-inline (loop for x in code
                      append (if (equal x '(:global.set :img-objs))
                                 '((:local.tee objs) (:global.set :img-objs))
                                 (list x))))))
;; image_module_snap(i): what RESTORE of the Ith module takes
(defhelper :image-module-snap :i-e ()
  '(:global.get :img-roots) '(:ref.cast! :vector) '(:i32.const 1) '(:array.get :vector) '(:ref.cast! :vector)
  '(:local.get 0) '(:array.get :vector))
;; image_resume(): the mutable globals again, then SYS::%IMAGE-RESUME
(defhelper :image-resume :v-v ()
  '(:global.get :img-roots) '(:ref.cast! :vector) '(:i32.const 0) '(:array.get :vector)
  '(:call :set-image-mutables)
  '(:global.get :img-roots)
  (lisp-call-instrs 'clysm-sys::%image-resume 1)
  :drop
  '(:ref.null :vector) '(:global.set :img-objs))

;;; helpers of structures whose slots are fields (see %INSTANCE-REF above;
;;; the slot vector of such a structure is the vector of the structures its
;;; class includes, LAYOUT-INHERITS)
(defhelper :instance-ref :ee-e ((sv (:ref :null :vector)) (k :i32))
  '(:local.get 1) '(:ref.cast :i31) :i31.get_s '(:local.set k)
  '(:local.get 0) '(:ref.test :sfield) '(:if nil nil ((:local.get 0) (:local.get k) (:call :sinst-ref) :return))
  '(:local.get 0) '(:ref.cast :instance) '(:struct.get :instance 1) '(:local.tee sv) :array.len '(:local.get k) :i32.gt_u
  '(:if nil nil ((:local.get sv) (:local.get k) (:array.get :vector) :return))
  '(:local.get 0) '(:local.get k) '(:call :sinst-ref))
(defhelper :instance-set :eee-e ((sv (:ref :null :vector)) (k :i32))
  '(:local.get 1) '(:ref.cast :i31) :i31.get_s '(:local.set k)
  '(:local.get 0) '(:ref.test :sfield)
  '(:if nil nil ((:local.get 0) (:local.get k) (:local.get 2) (:call :sinst-set) :return))
  '(:local.get 0) '(:ref.cast :instance) '(:struct.get :instance 1) '(:local.tee sv) :array.len '(:local.get k) :i32.gt_u
  '(:if nil nil ((:local.get sv) (:local.get k) (:local.get 2) (:array.set :vector) (:local.get 2) :return))
  '(:local.get 0) '(:local.get k) '(:local.get 2) '(:call :sinst-set))
;; the vector of the structures the class L (a layout) includes, by depth
;; (+CLASS-INHERITS+), or the empty one: the slot vector of a structure of
;; class L whose slots are fields, so that the accessors of the structures it
;; includes find them in it, one load from the instance
(defhelper :layout-inherits :e-vec ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :instance))
     (:struct.get :instance 1) (:i32.const ,+class-inherits-slot+) (:array.get :vector)
     (:br_on_cast_fail fail :eqref (:ref :vector)) :return)
  :drop '(:global.get :empty-slots))

;; (a structure with typed slots, src/typed.lisp, is not of the $siK types
;; past its leading slots of type T: the Lisp function, by its class's functions)
(defhelper :sinst-ref :ei-e ()
  `((:block typed :eqref
      ,@(sinst-dispatch-code 1 (lambda (j) `((:local.get 0) (:br_on_cast_fail typed :eqref (:ref ,(sinst-type (1+ j))))
                                             (:struct.get ,(sinst-type (1+ j)) ,(sinst-field j)) :return))
                             `((:local.get 1) (:i32.const ,+struct-inline-max+) :i32.ge_s
                               (:if nil nil ((:local.get 0) (:call :big-overflow) :ref.is_null :i32.eqz
                                             (:if nil nil ((:local.get 0) (:local.get 1) (:call :big-ref) :return))))
                               (:local.get 0) (:br typed))))
    :drop)
  '(:local.get 0) '(:local.get 1) :ref.i31 (lisp-call-instrs 'clysm-sys::%tstruct-gref 2))
(defhelper :sinst-set :eie-e ()
  `((:block typed :eqref
      ,@(sinst-dispatch-code 1 (lambda (j) `((:local.get 0) (:br_on_cast_fail typed :eqref (:ref ,(sinst-type (1+ j))))
                                             (:local.get 2) (:struct.set ,(sinst-type (1+ j)) ,(sinst-field j))
                                             (:local.get 2) :return))
                             `((:local.get 1) (:i32.const ,+struct-inline-max+) :i32.ge_s
                               (:if nil nil ((:local.get 0) (:call :big-overflow) :ref.is_null :i32.eqz
                                             (:if nil nil ((:local.get 0) (:local.get 1) (:local.get 2)
                                                           (:call :big-set) :return))))
                               (:local.get 0) (:br typed))))
    :drop)
  '(:local.get 0) '(:local.get 1) :ref.i31 '(:local.get 2) (lisp-call-instrs 'clysm-sys::%tstruct-gset 3))
;; (of a big structure, :BIG above: its fields and its overflow vector)
(defhelper :sinst-size :e-i ((o :eqref))
  '(:local.get 0) `(:call :big-overflow) '(:local.tee o) :ref.is_null :i32.eqz
  `(:if nil nil ((:i32.const ,+struct-inline-max+) (:local.get o) (:ref.cast! :vector) :array.len :i32.add :return))
  (sinst-size-code 0 +struct-inline-max+))
;; the overflow vector of a big structure, else null
(defhelper :big-overflow :e-e ((iv (:ref :null :vector)) (n :i32) (o :eqref))
  '(:local.get 0) `(:ref.test ,(sinst-type +struct-inline-max+)) :i32.eqz '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get 0) `(:ref.cast! ,(sinst-type +struct-inline-max+)) `(:struct.get ,(sinst-type +struct-inline-max+) 1)
  '(:local.tee iv) :array.len '(:local.tee n) :i32.eqz '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get iv) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :vector) '(:local.tee o) '(:ref.test :vector)
  '(:if nil nil ((:local.get o) :return))
  '(:ref.null :eq))
;; the slot vector of a big structure of class L with the overflow vector O
(defhelper :big-inherits :ee-e ((iv (:ref :null :vector)) (v (:ref :null :vector)) (n :i32))
  '(:local.get 0) '(:call :layout-inherits) '(:local.tee iv) :array.len '(:local.tee n) '(:i32.const 1) :i32.add
  '(:array.new_default :vector) '(:local.tee v) '(:i32.const 0) '(:local.get iv) '(:i32.const 0) '(:local.get n)
  '(:array.copy :vector :vector)
  '(:local.get v) '(:local.get n) '(:local.get 1) '(:array.set :vector)
  '(:local.get v))
;; slot K (at least +STRUCT-INLINE-MAX+) of a big structure
(defhelper :big-ref :ei-e ()
  '(:local.get 0) '(:call :big-overflow) '(:ref.cast :vector)
  '(:local.get 1) `(:i32.const ,+struct-inline-max+) :i32.sub '(:array.get :vector))
(defhelper :big-set :eie-e ()
  '(:local.get 0) '(:call :big-overflow) '(:ref.cast :vector)
  '(:local.get 1) `(:i32.const ,+struct-inline-max+) :i32.sub '(:local.get 2) '(:array.set :vector)
  '(:local.get 2))
;; a new structure of local 0 field slots (from 1), its fields unset
(defhelper :sinst-make :i-e ((k :i32))
  '(:local.get 0) '(:i32.const 1) :i32.sub '(:local.set k)
  (sinst-dispatch-code 'k (lambda (j) `((:ref.null :eq) (:global.get :empty-slots) (:i32.const 0)
                                        ,@(loop repeat (1+ j) collect '(:ref.null :eq))
                                        (:struct.new ,(sinst-type (1+ j))) :return))
                       ;; more: a big structure (its slot vector the overflow vector only)
                       `((:ref.null :eq) (:ref.null :eq) (:local.get 0) (:i32.const ,+struct-inline-max+) :i32.sub
                         (:array.new :vector) (:array.new_fixed :vector 1) (:i32.const 0)
                         ,@(loop repeat +struct-inline-max+ collect '(:ref.null :eq))
                         (:struct.new ,(sinst-type +struct-inline-max+)))))
;; a copy of a structure of field slots
(defhelper :sinst-copy :e-e ((k :i32))
  '(:local.get 0) '(:call :sinst-size) '(:i32.const 1) :i32.sub '(:local.set k)
  (sinst-dispatch-code 'k (lambda (j)
                            (let ((ty (sinst-type (1+ j))))
                              `((:local.get 0) (:ref.cast ,ty) (:struct.get ,ty 0)
                                (:local.get 0) (:ref.cast ,ty) (:struct.get ,ty 1)
                                (:i32.const 0)
                                ,@(loop for f below (1+ j)
                                        append `((:local.get 0) (:ref.cast ,ty) (:struct.get ,ty ,(sinst-field f))))
                                (:struct.new ,ty) :return)))
                       ;; a big structure: a copy of its overflow vector too
                       (let ((ty (sinst-type +struct-inline-max+)))
                         `((:local.get 0) (:ref.cast ,ty) (:struct.get ,ty 0)
                           (:local.get 0) (:ref.cast ,ty) (:struct.get ,ty 1) (:call :big-iv-copy) (:ref.cast :vector)
                           (:i32.const 0)
                           ,@(loop for f below +struct-inline-max+
                                   append `((:local.get 0) (:ref.cast ,ty) (:struct.get ,ty ,(sinst-field f))))
                           (:struct.new ,ty)))))
;; a copy of the slot vector of a big structure with a copy of its overflow vector
(defhelper :big-iv-copy :e-e ((iv (:ref :null :vector)) (v (:ref :null :vector)) (n :i32) (o (:ref :null :vector)))
  '(:local.get 0) '(:ref.cast :vector) '(:local.tee iv) :array.len '(:local.tee n) '(:array.new_default :vector)
  '(:local.tee v) '(:i32.const 0) '(:local.get iv) '(:i32.const 0) '(:local.get n) '(:array.copy :vector :vector)
  '(:local.get iv) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :vector) '(:ref.cast :vector) '(:local.tee o)
  :array.len '(:array.new_default :vector) '(:local.set iv)
  '(:local.get iv) '(:i32.const 0) '(:local.get o) '(:i32.const 0) '(:local.get o) :array.len '(:array.copy :vector :vector)
  '(:local.get v) '(:local.get n) '(:i32.const 1) :i32.sub '(:local.get iv) '(:array.set :vector)
  '(:local.get v))
