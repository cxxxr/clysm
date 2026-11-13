;;;; typed.lisp — code generation using the inferred types (types.lisp)
;;;;
;;;; A node whose type is a double-float is computed in an f64 and a node
;;;; whose type is a fixnum in an i32, without boxes and without the type
;;;; tests of the generic code; comparisons of such values are single Wasm
;;;; instructions.  Float operations whose result is infinite, NaN or zero
;;;; (where the generic code may signal FLOATING-POINT-OVERFLOW or -UNDERFLOW
;;;; or DIVISION-BY-ZERO) are redone by the generic function.

(in-package :clysm)

(defun node-op-name (node)
  (case (car node)
    (:prim (prim-name (second node)))
    (:call (second node))))

(defun cg-rep (node rep)
  "Instructions leaving the value of NODE in representation REP (:eqref, :f64
or :i32).  For :f64 / :i32 the type of NODE must be a double / fixnum."
  (if (eq rep :eqref)
      (cg node)
      (or (cg-rep-1 node rep)
          `(,@(cg node) ,@(unbox-rep-code rep (node-ty node))))))

(defun cg-rep-1 (node rep)
  (case (car node)
    (:const (let ((v (second node)))
              (ecase rep
                (:f64 (when (typep v 'double-float) `((:f64.const ,v))))
                (:i32 (when (typep v '(signed-byte 32)) `((:i32.const ,v))))
                (:i64 (when (typep v '(signed-byte 64)) `((:i64.const ,v)))))))
    (:lref (let ((lv (second node)))
             (when (unboxed-local-p lv)
               (let ((get `(:local.get ,(cdr (get-loc lv)))))
                 (cond ((eq (lvar-rep lv) rep) (list get))
                       ((and (eq (lvar-rep lv) :i64) (eq rep :i32)) (list get :i32.wrap_i64))
                       ((and (eq (lvar-rep lv) :i32) (eq rep :i64)) (list get :i64.extend_i32_s)))))))
    (:lset (let ((lv (second node)))
             (when (and (eq (lvar-rep lv) rep) (unboxed-local-p lv))
               `(,@(cg-rep (third node) rep) (:local.tee ,(cdr (get-loc lv)))))))
    (:the (cg-rep (third node) rep))
    (:progn (when (cdr node)
              `(,@(loop for n in (butlast (cdr node)) append (cg-effect n))
                ,@(cg-rep (car (last node)) rep))))
    (:if `(,@(cg-test (second node))
           (:if nil ,(rep-wasm-type rep) ,(cg-rep (third node) rep) ,(cg-rep (fourth node) rep))))
    (:let (cg-let node nil nil rep))
    ((:prim :call) (ecase rep
                     (:i64 (cond ((ty-int32-p (node-ty node)) `(,@(cg-rep node :i32) :i64.extend_i32_s))
                                 ((ty-int64-p (node-ty node)) (i64-op-code node))))
                     (:f64 (when (ty-double-p (node-ty node)) (f64-op-code node)))
                     (:i32 (when (ty-int32-p (node-ty node))
                             (or (i32-op-code node)
                                 (when (fused-tree-node-p node) (fused-code node :i32)))))))
    (t nil)))

;;; ------------------------------------------------------------
;;; f64
;;; ------------------------------------------------------------

(defun f64-arg-p (node &optional exact)
  "NODE can be converted to an f64 inline (EXACT: without rounding, as
comparisons need)."
  (let ((ty (node-ty node)))
    (or (ty-double-p ty) (ty-int32-p ty)
        (and (not exact) (ty-only-p ty +k-real+)))))

(defun f64-arg-code (node)
  (let ((ty (node-ty node)))
    (cond ((ty-double-p ty) (cg-rep node :f64))
          ((ty-int32-p ty) `(,@(cg-rep node :i32) :f64.convert_i32_s))
          (t `(,@(cg node) (:call :to-f64))))))

(defparameter *f64-generic*
  '((+ . clysm-sys::%generic-add) (- . clysm-sys::%generic-sub)
    (* . clysm-sys::%generic-mul) (/ . clysm-sys::%generic-div)))

(defun f64-binop-code (name a-code b-code)
  "A-CODE and B-CODE leave f64s."
  (let ((x (new-local :f64)) (y (new-local :f64)) (r (new-local :f64))
        (op (ecase name (+ :f64.add) (- :f64.sub) (* :f64.mul) (/ :f64.div))))
    `(,@a-code (:local.set ,x) ,@b-code (:local.set ,y)
      (:local.get ,x) (:local.get ,y) ,op (:local.tee ,r)
      ;; not finite (or zero for * and /): the generic function decides
      (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne
      ;; (a zero result of nonzero operands may be an underflow)
      ,@(when (eq name '*) `((:local.get ,r) (:f64.const 0) :f64.eq
                             (:local.get ,x) (:f64.const 0) :f64.ne :i32.and
                             (:local.get ,y) (:f64.const 0) :f64.ne :i32.and :i32.or))
      ,@(when (eq name '/) `((:local.get ,r) (:f64.const 0) :f64.eq
                             (:local.get ,x) (:f64.const 0) :f64.ne :i32.and :i32.or))
      (:if nil :f64
           ((:local.get ,x) (:struct.new :double) (:local.get ,y) (:struct.new :double)
            ,@(lisp-call-instrs (cdr (assoc name *f64-generic*)) 2)
            (:ref.cast :double) (:struct.get :double 0))
           ((:local.get ,r))))))

(defun f64-op-code (node)
  "f64 code for an operation whose result is a double, or NIL."
  (let ((name (node-op-name node)) (args (cddr node)))
    (case name
      ((+ - * /)
       (cond ((and (= (length args) 2) (every #'f64-arg-p args))
              (f64-binop-code name (f64-arg-code (first args)) (f64-arg-code (second args))))
             ((and (eq name '-) (= (length args) 1) (ty-double-p (node-ty (first args))))
              `(,@(cg-rep (first args) :f64) :f64.neg))))
      ((1+ 1-)
       (when (f64-arg-p (first args))
         (f64-binop-code (if (eq name '1+) '+ '-) (f64-arg-code (first args)) '((:f64.const 1d0)))))
      (sqrt (let ((ty (node-ty (first args))))
              (when (and (ty-double-p ty) (ty-nonneg-p ty))
                `(,@(cg-rep (first args) :f64) :f64.sqrt))))
      (abs (when (ty-double-p (node-ty (first args)))
             `(,@(cg-rep (first args) :f64) :f64.abs)))
      (float (let ((x (first args)) (proto (second args)))
               (when (and (f64-arg-p x)
                          (or (and proto (ty-double-p (node-ty proto)))
                              (and (null proto) (ty-double-p (node-ty x)))))
                 (if (and proto (not (eq (car proto) :const)))
                     (let ((tmp (new-local :f64)))
                       `(,@(f64-arg-code x) (:local.set ,tmp) ,@(cg-effect proto) (:local.get ,tmp)))
                     (f64-arg-code x)))))
      (t (numeric-f64-op-code node)))))

;;; ------------------------------------------------------------
;;; i32 (fixnums)
;;; ------------------------------------------------------------

(defun fx-arg-p (node) (ty-fixnum-p (node-ty node)))
(defun i32-arg-p (node) (ty-int32-p (node-ty node)))

(defun const-int-node (node)
  (let ((ty (node-ty node)))
    (and (ty-only-p ty +k-int+) (ty-lo ty) (eql (ty-lo ty) (ty-hi ty)) (ty-lo ty))))

(defun i32-op-code (node)
  "i32 code for an operation whose result is a fixnum, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node))
         (a (first args)) (b (second args)))
    (flet ((both (op) (when (and (= (length args) 2) (i32-arg-p a) (i32-arg-p b))
                        `(,@(cg-rep a :i32) ,@(cg-rep b :i32) ,op))))
      (case name
        (+ (both :i32.add))
        (- (if (= (length args) 1)
               (when (i32-arg-p a) `((:i32.const 0) ,@(cg-rep a :i32) :i32.sub))
               (both :i32.sub)))
        (* (both :i32.mul))
        (1+ (when (i32-arg-p a) `(,@(cg-rep a :i32) (:i32.const 1) :i32.add)))
        (1- (when (i32-arg-p a) `(,@(cg-rep a :i32) (:i32.const 1) :i32.sub)))
        ((logand clysm-sys::%fxlogand) (both :i32.and))
        ((logior clysm-sys::%fxlogior) (both :i32.or))
        ((logxor clysm-sys::%fxlogxor) (both :i32.xor))
        (clysm-sys::%fx+ (both :i32.add))
        (clysm-sys::%fx- (both :i32.sub))
        (clysm-sys::%fx* (both :i32.mul))
        (ash (let ((c (and b (const-int-node b))))
               (when (and c (i32-arg-p a) (<= -64 c 31))
                 (if (>= c 0)
                     `(,@(cg-rep a :i32) ,@(cg-effect b) (:i32.const ,c) :i32.shl)
                     `(,@(cg-rep a :i32) ,@(cg-effect b) (:i32.const ,(min 31 (- c))) :i32.shr_s)))))
        ((char-code clysm-sys::%char-code)
         (when (ty-only-p (node-ty a) +k-char+)
           `(,@(cg a) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.sub)))
        (clysm-sys::%lref (when (ty-only-p (node-ty a) +k-limbs+)
                            `(,@(cg a) (:ref.cast! :limbs) ,@(index-i32-code b) (:array.get :limbs))))
        (clysm-sys::%llen (when (ty-only-p (node-ty a) +k-limbs+) `(,@(cg a) (:ref.cast! :limbs) :array.len)))
        ((length clysm-sys::%svlength clysm-sys::%string-length)
         (let ((ty (node-ty a)))
           (cond ((ty-only-p ty +k-svec+) `(,@(cg a) (:ref.cast! :vector) :array.len))
                 ((ty-only-p ty +k-string+) `(,@(cg a) (:ref.cast! :string) :array.len)))))
        ((mod rem truncate floor)
         (when (and (= (length args) 2) (fx-arg-p a) (fx-arg-p b))
           (let* ((bt (node-ty b)) (nonzero (or (> (ty-lo bt) 0) (< (ty-hi bt) 0)))
                  (x (new-local :i32)) (y (new-local :i32)) (r (new-local :i32)))
             (when nonzero
               (let ((pre `(,@(cg-rep a :i32) (:local.set ,x) ,@(cg-rep b :i32) (:local.set ,y))))
                 (ecase name
                   (rem `(,@pre (:local.get ,x) (:local.get ,y) :i32.rem_s))
                   (truncate (when (or (> (ty-lo bt) 0) (> (ty-lo (node-ty a)) +most-negative-fixnum+))
                               `(,@pre (:local.get ,x) (:local.get ,y) :i32.div_s)))
                   (mod (if (and (>= (ty-lo (node-ty a)) 0) (> (ty-lo bt) 0))
                            `(,@pre (:local.get ,x) (:local.get ,y) :i32.rem_s)
                            `(,@pre (:local.get ,x) (:local.get ,y) :i32.rem_s (:local.set ,r)
                              (:local.get ,r) (:local.get ,y) :i32.xor (:i32.const 0) :i32.lt_s
                              (:local.get ,r) (:i32.const 0) :i32.ne :i32.and   ; r /= 0 and the signs differ
                              (:if nil :i32 ((:local.get ,r) (:local.get ,y) :i32.add) ((:local.get ,r))))))
                   (floor (when (or (> (ty-lo bt) 0) (> (ty-lo (node-ty a)) +most-negative-fixnum+))
                            `(,@pre (:local.get ,x) (:local.get ,y) :i32.div_s
                              (:local.get ,x) (:local.get ,y) :i32.rem_s (:local.set ,r)
                              (:local.get ,r) (:local.get ,y) :i32.xor (:i32.const 0) :i32.lt_s
                              (:local.get ,r) (:i32.const 0) :i32.ne :i32.and
                              :i32.sub)))))))))   ; q - 1 when the remainder has the wrong sign
        (t (numeric-i32-op-code node))))))

;;; ------------------------------------------------------------
;;; eqref results and tests
;;; ------------------------------------------------------------

(defun i64-arg-p (node) (ty-int64-p (node-ty node)))

(defun i64-op-code (node)
  "i64 code for an integer operation whose result is within 64 bits, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (flet ((both (op) (when (and (= (length args) 2) (i64-arg-p a) (i64-arg-p b))
                        `(,@(cg-rep a :i64) ,@(cg-rep b :i64) ,op))))
      (case name
        (+ (both :i64.add))
        (- (if (= (length args) 1)
               (when (i64-arg-p a) `((:i64.const 0) ,@(cg-rep a :i64) :i64.sub))
               (both :i64.sub)))
        (* (both :i64.mul))
        (1+ (when (i64-arg-p a) `(,@(cg-rep a :i64) (:i64.const 1) :i64.add)))
        (1- (when (i64-arg-p a) `(,@(cg-rep a :i64) (:i64.const 1) :i64.sub)))
        (logand (both :i64.and))
        (logior (both :i64.or))
        (logxor (both :i64.xor))
        (ash (let ((c (and b (const-int-node b))))
               (when (and c (i64-arg-p a) (<= -64 c 63))
                 (if (>= c 0)
                     `(,@(cg-rep a :i64) ,@(cg-effect b) (:i64.const ,c) :i64.shl)
                     `(,@(cg-rep a :i64) ,@(cg-effect b) (:i64.const ,(min 63 (- c))) :i64.shr_s)))))
        (t (numeric-i64-op-code node))))))

(defun typed-cg (node mv)
  "Code for a :PRIM or :CALL node using its types, or NIL."
  (let ((ty (node-ty node)) (name (node-op-name node)) (args (cddr node)))
    (when (and mv (member name '(floor truncate ceiling round)))
      (return-from typed-cg nil))
    (let ((code
            (cond ((and (eq (car node) :prim) (eq (prim-kind (second node)) :test))
                   (let ((c (typed-test-code node))) (when c `(,@c ,@(bool-code)))))
                  ((numeric-typed-code node mv))
                  ((member name '(clysm-sys::%aref1 svref clysm-sys::%aset1 clysm-sys::%svset char schar))
                   (typed-ref-code node))
                  ((member name '(clysm-sys::%struct-ref clysm-sys::%struct-set))
                   (typed-struct-code node))
                  ((eq name 'clysm-sys::%aref2) (typed-aref2-code node))
                  ((member name '(car cdr first rest clysm-sys::%lref clysm-sys::%lset
                                  clysm-sys::%svref clysm-sys::%set-svref))
                   (typed-access-code node))
                  ((ty-double-p ty)
                   (let ((c (f64-op-code node))) (when c `(,@c (:struct.new :double)))))
                  ((ty-int32-p ty)
                   (let ((c (i32-op-code node))) (when c `(,@c ,@(box-rep-code :i32 ty)))))
                  ((and (ty-int64-p ty) (not (every #'i32-arg-p args)))
                   (let ((c (i64-op-code node))) (when c `(,@c ,@(box-rep-code :i64 ty)))))
                  ((and (member name '(+ - * 1+ 1-)) (ty-only-p ty +k-int+)
                        (every #'i32-arg-p args) (<= 1 (length args) 2)
                        (not (and (eq name '-) (= (length args) 1))))
                   ;; fixnum arguments: compute in i64, box a bignum when needed
                   (let ((r (new-local :i64)))
                     `(,@(cg-rep (first args) :i32) :i64.extend_i32_s
                       ,@(case name
                           ((1+ 1-) `((:i64.const 1) ,(if (eq name '1+) :i64.add :i64.sub)))
                           (t `(,@(cg-rep (second args) :i32) :i64.extend_i32_s
                                ,(ecase name (+ :i64.add) (- :i64.sub) (* :i64.mul)))))
                       (:local.tee ,r) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                       (:if nil :eqref ((:local.get ,r) :i32.wrap_i64 :ref.i31)
                            ((:local.get ,r) (:call :i64-int))))))
                  (t nil))))
      (when code (append code (when mv (mv-one)))))))

(defun typed-ref-code (node)
  "Element access of a simple vector / string known to be one, with a
fixnum index: inline with a bounds check (else the generic code, which signals)."
  (let* ((name (node-op-name node)) (args (cddr node))
         (a (first args)) (i (second args)) (val (third args))
         (aty (and a (node-ty a))))
    (when (and a i (fx-arg-p i))
      (let ((v (new-local)) (k (new-local :i32)) (x (new-local)))
        (flet ((guarded (cast len-type fast slow)
                 `(,@(cg a) (:local.set ,v) ,@(cg-rep i :i32) (:local.set ,k)
                   ,@(when val `(,@(cg val) (:local.set ,x)))
                   (:local.get ,k) (:local.get ,v) (:ref.cast! ,cast) :array.len :i32.lt_u
                   (:if nil :eqref ,fast ,slow)
                   ,@(progn len-type nil))))
          (case name
            ((clysm-sys::%aref1 svref)
             (when (ty-only-p aty +k-svec+)
               (guarded :vector :vector
                        `((:local.get ,v) (:ref.cast! :vector) (:local.get ,k) (:array.get :vector))
                        `((:local.get ,v) (:local.get ,k) :ref.i31 (:call :aref1)))))
            ((clysm-sys::%aset1 clysm-sys::%svset)
             (when (and val (ty-only-p aty +k-svec+))
               (guarded :vector :vector
                        `((:local.get ,v) (:ref.cast! :vector) (:local.get ,k) (:local.get ,x) (:array.set :vector)
                          (:local.get ,x))
                        `((:local.get ,v) (:local.get ,k) :ref.i31 (:local.get ,x) (:call :aset1)))))
            ((char schar)
             (when (ty-only-p aty +k-string+)
               (guarded :string :string
                        `((:local.get ,v) (:ref.cast! :string) (:local.get ,k) (:array.get :string)
                          (:i32.const ,+char-tag+) :i32.add :ref.i31)
                        `((:local.get ,v) (:local.get ,k) :ref.i31 (:call :schar-ref)))))))))))

(defun index-i32-code (node)
  "An index known to be a fixnum (unchecked) as an i32."
  (if (fx-arg-p node) (cg-rep node :i32) `(,@(cg node) (:ref.cast! :i31) :i31.get_s)))

(defun typed-access-code (node)
  "CAR / CDR of a list or cons, limb and simple-vector access with the type known."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (aty (node-ty a)))
    (case name
      ((car cdr first rest)
       (let ((field (if (member name '(car first)) 0 1)))
         (cond ((ty-only-p aty +k-cons+) `(,@(cg a) (:ref.cast! :cons) (:struct.get :cons ,field)))
               ((ty-only-p aty +k-list+)
                (let ((tmp (new-local)))
                  `(,@(cg a) (:local.tee ,tmp) (:ref.test :cons)
                    (:if nil :eqref ((:local.get ,tmp) (:ref.cast! :cons) (:struct.get :cons ,field))
                         ,(nil-code)))))
               (t
                ;; a cons inline, anything else by the helper (NIL, or the type error)
                (let ((tmp (new-local)))
                  `(,@(cg a) (:local.tee ,tmp) (:ref.test :cons)
                    (:if nil :eqref ((:local.get ,tmp) (:ref.cast! :cons) (:struct.get :cons ,field))
                         ((:local.get ,tmp) (:call ,(if (= field 0) :car :cdr))))))))))
      (clysm-sys::%lref
       (when (ty-only-p aty +k-limbs+)
         `(,@(cg a) (:ref.cast! :limbs) ,@(index-i32-code (second args)) (:array.get :limbs) :ref.i31)))
      (clysm-sys::%lset
       (when (ty-only-p aty +k-limbs+)
         (let ((tmp (new-local :i32)))
           `(,@(cg a) (:ref.cast! :limbs) ,@(index-i32-code (second args))
             ,@(index-i32-code (third args)) (:local.tee ,tmp) (:array.set :limbs)
             (:local.get ,tmp) :ref.i31))))
      (clysm-sys::%svref
       (when (ty-only-p aty +k-svec+)
         `(,@(cg a) (:ref.cast! :vector) ,@(index-i32-code (second args)) (:array.get :vector))))
      (clysm-sys::%set-svref
       (when (ty-only-p aty +k-svec+)
         (let ((tmp (new-local)))
           `(,@(cg a) (:ref.cast! :vector) ,@(index-i32-code (second args))
             ,@(cg (third args)) (:local.tee ,tmp) (:array.set :vector) (:local.get ,tmp))))))))

(defun typed-aref2-code (node)
  "2-D AREF with fixnum indices: the helper's fast path inline."
  (destructuring-bind (a i j) (cddr node)
    (when (and (fx-arg-p i) (fx-arg-p j))
      (let ((av (new-local)) (x (new-local :i32)) (y (new-local :i32)) (m (new-local '(:ref :null :mdarray)))
            (dims (new-local)) (rest (new-local)) (d1 (new-local :i32)) (v (new-local '(:ref :null :vector)))
            (fail (gensym "SLOW")) (done (gensym "AREF2")))
        `(,@(cg a) (:local.set ,av) ,@(cg-rep i :i32) (:local.set ,x) ,@(cg-rep j :i32) (:local.set ,y)
          (:block ,done :eqref
            (:block ,fail nil
              (:local.get ,av) (:ref.test :mdarray) :i32.eqz (:br_if ,fail)
              (:local.get ,av) (:ref.cast! :mdarray) (:local.set ,m)
              (:local.get ,x) (:local.get ,m) (:struct.get :mdarray 5) :i32.ge_u (:br_if ,fail)
              (:local.get ,y) (:local.get ,m) (:struct.get :mdarray 6) (:local.tee ,d1) :i32.ge_u (:br_if ,fail)
              (:local.get ,m) (:struct.get :mdarray 0) (:ref.test :vector) :i32.eqz (:br_if ,fail)
              (:local.get ,m) (:struct.get :mdarray 0) (:ref.cast! :vector)
              (:local.get ,m) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s
              (:local.get ,x) (:local.get ,d1) :i32.mul :i32.add (:local.get ,y) :i32.add
              (:array.get :vector) (:br ,done))
            (:local.get ,av) (:local.get ,x) :ref.i31 (:local.get ,y) :ref.i31 (:call :aref2)))))))

(defun typed-struct-code (node)
  "Slot access of an object known to be of the structure class: no check."
  (destructuring-bind (x cls i &optional (v nil vp)) (cddr node)
    (let ((name (struct-class-node-name cls)))
      (when (and name (eq (ty-cls (node-ty x)) name) (fx-arg-p i))
        (if vp
            (let ((tmp (new-local)))
              `(,@(cg x) (:ref.cast! :instance) (:struct.get :instance 1) ,@(cg-rep i :i32)
                ,@(cg v) (:local.tee ,tmp) (:array.set :vector) (:local.get ,tmp)))
            `(,@(cg x) (:ref.cast! :instance) (:struct.get :instance 1) ,@(cg-rep i :i32)
              (:array.get :vector)))))))

(defun typed-test-code (node)
  "i32 code for a numeric comparison or predicate using the argument types, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node))
         (a (first args)) (b (second args)))
    (case name
      ((< > <= >= = /=)
       (when (= (length args) 2)
         (cond ((and (i32-arg-p a) (i32-arg-p b))
                `(,@(cg-rep a :i32) ,@(cg-rep b :i32)
                  ,(ecase name (< :i32.lt_s) (> :i32.gt_s) (<= :i32.le_s) (>= :i32.ge_s) (= :i32.eq) (/= :i32.ne))))
               ((and (i64-arg-p a) (i64-arg-p b) (or (lvar-rep-of a) (lvar-rep-of b)))
                `(,@(cg-rep a :i64) ,@(cg-rep b :i64)
                  ,(ecase name (< :i64.lt_s) (> :i64.gt_s) (<= :i64.le_s) (>= :i64.ge_s) (= :i64.eq) (/= :i64.ne))))
               ((and (f64-arg-p a t) (f64-arg-p b t)
                     (or (ty-double-p (node-ty a)) (ty-double-p (node-ty b))))
                `(,@(f64-arg-code a) ,@(f64-arg-code b)
                  ,(ecase name (< :f64.lt) (> :f64.gt) (<= :f64.le) (>= :f64.ge) (= :f64.eq) (/= :f64.ne)))))))
      ((zerop plusp minusp)
       (cond ((i32-arg-p a) `(,@(cg-rep a :i32) ,@(ecase name (zerop '(:i32.eqz))
                                                  (plusp '((:i32.const 0) :i32.gt_s))
                                                  (minusp '((:i32.const 0) :i32.lt_s)))))
             ((ty-double-p (node-ty a))
              `(,@(cg-rep a :f64) (:f64.const 0) ,(ecase name (zerop :f64.eq) (plusp :f64.gt) (minusp :f64.lt))))))
      ((evenp oddp)
       (when (i32-arg-p a)
         `(,@(cg-rep a :i32) (:i32.const 1) :i32.and ,@(when (eq name 'evenp) '(:i32.eqz)))))
      ((eq eql)
       (when (and (= (length args) 2) (fx-arg-p a) (fx-arg-p b)
                  (or (eq (lvar-rep-of a) :i32) (eq (lvar-rep-of b) :i32)))
         `(,@(cg-rep a :i32) ,@(cg-rep b :i32) :i32.eq)))
      (t (numeric-test-code node)))))

(defun typed-divmod-code (node)
  "For (FLOOR x y) / (TRUNCATE x y) of fixnums: code leaving the quotient (a
fixnum) whose remainder is left in an eqref local; returns (values code local)."
  (when (and (member (car node) '(:call :prim)) (member (node-op-name node) '(floor truncate))
             (= (length (cddr node)) 2) (ty-fixnum-p (node-ty node)))
    (destructuring-bind (a b) (cddr node)
      (let ((bt (node-ty b)))
        (when (and (fx-arg-p a) (fx-arg-p b) (or (> (ty-lo bt) 0) (< (ty-hi bt) 0)))
          (let ((x (new-local :i32)) (y (new-local :i32)) (q (new-local :i32)) (r (new-local :i32))
                (rem (new-local)))
            (values
             `(,@(cg-rep a :i32) (:local.set ,x) ,@(cg-rep b :i32) (:local.set ,y)
               (:local.get ,x) (:local.get ,y) :i32.div_s (:local.set ,q)
               (:local.get ,x) (:local.get ,y) :i32.rem_s (:local.set ,r)
               ,@(when (eq (node-op-name node) 'floor)
                   ;; a nonzero remainder of the other sign than the divisor
                   `((:local.get ,r) (:i32.const 0) :i32.ne
                     (:local.get ,r) (:local.get ,y) :i32.xor (:i32.const 0) :i32.lt_s :i32.and
                     (:if nil nil ((:local.get ,q) (:i32.const 1) :i32.sub (:local.set ,q)
                                   (:local.get ,r) (:local.get ,y) :i32.add (:local.set ,r)))))
               (:local.get ,r) :ref.i31 (:local.set ,rem)
               (:local.get ,q) :ref.i31 (:i32.const 2) (:global.set :mv-count))
             rem)))))))

(defun lvar-rep-of (node)
  (and (eq (car node) :lref) (unboxed-local-p (second node)) (lvar-rep (second node))))

;;; ------------------------------------------------------------
;;; Numeric library: typed code for the division functions and the bit
;;; operations (called from F64-OP-CODE, I32-OP-CODE, I64-OP-CODE, TYPED-CG and
;;; CG-MVBIND; the result types are in the "Numeric library" part of types.lisp)
;;; ------------------------------------------------------------

(defparameter *divmod-modes*
  '((truncate . 0) (floor . 1) (ceiling . 2) (round . 3)
    (ftruncate . 0) (ffloor . 1) (fceiling . 2) (fround . 3) (rem . 0) (mod . 1)))

(defun float-divmod-node-p (node)
  (and (member (car node) '(:call :prim))
       (assoc (node-op-name node) *divmod-modes*)
       (float-divmod-types-p (mapcar #'node-ty (cddr node)))))

(defun typed-float-divmod (node want &optional need-rem)
  "Division of doubles (or a double and a 32-bit integer) inline, by the
algorithm of the helper :divmod, which is called when a value is not finite
or the divisor is zero.  WANT: :quotient (the integer quotient is left on the
stack), :fquotient (the quotient as an f64) or :remainder (nothing is left).
Returns the code and the f64 local that holds the remainder (it is not
computed for a division by 1 unless NEED-REM)."
  (let* ((args (cddr node)) (mode (cdr (assoc (node-op-name node) *divmod-modes*)))
         (da (new-local :f64)) (db (new-local :f64)) (q (new-local :f64)) (r (new-local :f64))
         (tmp (new-local :f64)) (adj (new-local :i32)) (w (new-local :i64))
         (slow (gensym "SLOW")) (done (gensym "DIV"))
         (unit (null (second args))))
    (if (and unit (not need-rem) (not (eq want :remainder)))
        ;; one argument, the quotient only: the rounding instruction (the
        ;; remainder of a division by 1 is exact, so truncating and
        ;; adjusting by it gives the same integer)
        (let ((op (ecase mode (0 :f64.trunc) (1 :f64.floor) (2 :f64.ceil) (3 :f64.nearest))))
          (ecase want
            (:quotient
             `(,@(f64-arg-code (first args)) ,op (:local.tee ,q) :f64.abs (:f64.const 536870912d0) :f64.lt
               (:if nil :eqref
                    ((:local.get ,q) :i32.trunc_sat_f64_s :ref.i31)
                    ;; (signals when it is not finite)
                    ((:local.get ,q) (:call :f64-int)))))
            (:fquotient
             `(,@(f64-arg-code (first args)) (:local.tee ,da) ,op (:f64.const 0) :f64.add (:local.tee ,q)
               (:local.get ,q) :f64.sub (:f64.const 0) :f64.ne
               (:if nil :f64
                    ((:local.get ,da) (:struct.new :double) ,@(fixnum-code 1) (:i32.const ,mode) (:call :divmod)
                     (:call :to-f64))
                    ((:local.get ,q)))))))
        (values
         `(,@(f64-arg-code (first args)) (:local.set ,da)
           ,@(if unit '((:f64.const 1d0)) (f64-arg-code (second args))) (:local.set ,db)
           (:block ,done ,(ecase want (:quotient :eqref) (:fquotient :f64) (:remainder nil))
             (:block ,slow nil
               ,@(float-divmod-code mode slow da db q r adj nil tmp unit)
               ,@(ecase want
                   (:quotient
                    `((:local.get ,q) :f64.abs (:f64.const 536870910d0) :f64.lt
                      (:if nil :eqref
                           ((:local.get ,q) :i32.trunc_sat_f64_s (:local.get ,adj) :i32.add :ref.i31)
                           ((:local.get ,q) :f64.abs (:f64.const 4.611686018427388d18) :f64.lt :i32.eqz (:br_if ,slow)
                            (:local.get ,q) :i64.trunc_sat_f64_s (:local.get ,adj) :i64.extend_i32_s :i64.add
                            (:call :i64-int)))))
                   (:fquotient `((:local.get ,q) (:local.get ,adj) :f64.convert_i32_s :f64.add))
                   (:remainder nil))
               (:br ,done))
             ;; the helper decides (and signals)
             (:local.get ,da) (:struct.new :double) (:local.get ,db) (:struct.new :double)
             (:i32.const ,mode) (:call :divmod)
             ,@(ecase want
                 (:quotient nil)
                 (:fquotient '((:call :to-f64)))
                 (:remainder '(:drop)))
             (:global.get :mv-buf) (:i32.const 0) (:array.get :vector) (:ref.cast :double) (:struct.get :double 0)
             (:local.set ,r)))
         r))))

(defun numeric-typed-code (node mv)
  "Code for a :PRIM or :CALL node of a numeric function (an eqref), or NIL."
  (let ((name (node-op-name node)))
    (cond ((and (not mv) (member name '(floor truncate ceiling round)) (float-divmod-node-p node))
           (values (typed-float-divmod node :quotient)))
          (t nil))))

(defun numeric-f64-op-code (node)
  "f64 code for a numeric function whose result is a double, or NIL."
  (let ((name (node-op-name node)))
    (cond ((and (member name '(mod rem)) (float-divmod-node-p node))
           (multiple-value-bind (code r) (typed-float-divmod node :remainder)
             `(,@code (:local.get ,r))))
          ((and (member name '(ffloor fceiling ftruncate fround)) (float-divmod-node-p node))
           (values (typed-float-divmod node :fquotient)))
          (t nil))))

(defun numeric-i32-op-code (node)
  "i32 code for a numeric function whose result is within 32 bits, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)))
    (case name
      ((integer-length logcount)
       ;; of X when it is not negative, else of (LOGNOT X)
       (cond ((i32-arg-p a)
              (let ((x (new-local :i32)))
                `(,@(cg-rep a :i32) (:local.tee ,x) (:local.get ,x) (:i32.const 31) :i32.shr_s :i32.xor
                  ,@(if (eq name 'logcount)
                        '(:i32.popcnt)
                        (let ((n (new-local :i32)))
                          `(:i32.clz (:local.set ,n) (:i32.const 32) (:local.get ,n) :i32.sub))))))
             ((i64-arg-p a)
              (let ((x (new-local :i64)))
                `(,@(cg-rep a :i64) (:local.tee ,x) (:local.get ,x) (:i64.const 63) :i64.shr_s :i64.xor
                  ,@(if (eq name 'logcount)
                        '(:i64.popcnt :i32.wrap_i64)
                        (let ((n (new-local :i32)))
                          `(:i64.clz :i32.wrap_i64 (:local.set ,n) (:i32.const 64) (:local.get ,n) :i32.sub))))))))
      (abs (when (i32-arg-p a)
             (let ((x (new-local :i32)) (m (new-local :i32)))
               `(,@(cg-rep a :i32) (:local.tee ,x) (:i32.const 31) :i32.shr_s (:local.tee ,m)
                 (:local.get ,x) :i32.xor (:local.get ,m) :i32.sub))))
      (isqrt
       ;; exact: the argument converts exactly and the square root is correctly rounded
       (let ((ty (node-ty a)))
         (when (and (i32-arg-p a) (>= (ty-lo ty) 0))
           `(,@(cg-rep a :i32) :f64.convert_i32_s :f64.sqrt :i32.trunc_sat_f64_s))))
      (t
       ;; a result within 32 bits of operands within 64 (a mask, a right
       ;; shift, a difference): the i64 operation, wrapped
       (when (and (ty-int32-p (node-ty node)) (some #'i64-arg-p args)
                  (notevery #'i32-arg-p args))
         (let ((c (i64-op-code node))) (when c `(,@c :i32.wrap_i64))))))))

(defun numeric-test-code (node)
  "i32 code for a numeric predicate using the argument types, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (case name
      ((zerop plusp minusp)
       (when (i64-arg-p a)
         `(,@(cg-rep a :i64) ,@(ecase name (zerop '(:i64.eqz))
                                 (plusp '((:i64.const 0) :i64.gt_s))
                                 (minusp '((:i64.const 0) :i64.lt_s))))))
      ((evenp oddp)
       (when (i64-arg-p a)
         `(,@(cg-rep a :i64) :i32.wrap_i64 (:i32.const 1) :i32.and ,@(when (eq name 'evenp) '(:i32.eqz)))))
      (logbitp
       ;; a constant index into an integer within 64 bits
       (let ((k (and a (const-int-node a))))
         (when (and k (>= k 0) b (i64-arg-p b))
           `(,@(cg-effect a) ,@(cg-rep b :i64) (:i64.const ,(min k 63)) :i64.shr_s :i32.wrap_i64
             (:i32.const 1) :i32.and))))
      (t nil))))

(defun numeric-i64-op-code (node)
  "i64 code for a numeric function whose result is within 64 bits, or NIL."
  (declare (ignorable node))
  nil)

(defun numeric-divmod-code (vnode)
  "For MULTIPLE-VALUE-BIND of a division: code leaving the quotient (an
eqref), the local of the remainder and its representation; or NIL."
  (when (float-divmod-node-p vnode)
    (let ((name (node-op-name vnode)))
      (cond ((member name '(floor truncate ceiling round))
             (multiple-value-bind (code r) (typed-float-divmod vnode :quotient t)
               (values code r :f64)))
            ((member name '(ffloor fceiling ftruncate fround))
             (multiple-value-bind (code r) (typed-float-divmod vnode :fquotient t)
               (values `(,@code (:struct.new :double)) r :f64)))))))

;;; ------------------------------------------------------------
;;; Integer accumulators
;;; ------------------------------------------------------------
;;; A LET (or MULTIPLE-VALUE-BIND) variable that is assigned, not captured,
;;; and holds only integers, but not provably within 64 bits (a sum, a
;;; counter, a hash value), has an i64 local beside its eqref local.  When
;;; the eqref local is null the value is the i64.  (SETQ var (op var E)) and
;;; assignments of nested integer arithmetic compute in i64 and store there,
;;; so that a value between 2^29 and 2^63 is not boxed at every step; the
;;; first other use of the variable makes the integer and keeps it in the
;;; eqref local.  An overflow or an operand that is not an integer within 64
;;; bits takes the generic operation, whose result goes to the eqref local.

(defvar *dual-vars* (make-hash-table :test 'eq) "lvar -> index of its i64 local")

(defun note-dual-var (lv)
  "Called when LV gets its location (BIND-NEW-VAR-CODE)."
  (if (and (lvar-mutated lv) (not (lvar-captured lv)) (not (lvar-special lv)) (null (lvar-rep lv))
           (lvar-type lv) (ty-only-p (lvar-type lv) +k-int+))
      (setf (gethash lv *dual-vars*) (new-local :i64))
      (remhash lv *dual-vars*)))

(defun dual-local (lv)
  (and (null (lvar-rep lv)) (not (boxed-p lv)) (gethash lv *dual-vars*)))

(defun dual-materialize-code (b v)
  "The integer of the pair of locals B (eqref) and V (i64), kept in B."
  (let ((ok (gensym "DUAL")))
    `((:block ,ok (:ref :eq)
        (:local.get ,b) (:br_on_non_null ,ok)
        (:local.get ,v) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
        (:if nil :eqref ((:local.get ,v) :i32.wrap_i64 :ref.i31) ((:local.get ,v) (:call :i64-int)))
        (:local.tee ,b) :ref.as_non_null))))

(defun dual-get-code (lv)
  (dual-materialize-code (cdr (get-loc lv)) (dual-local lv)))

(defun dual-pair-i64-code (b v fail)
  `((:local.get ,b) :ref.is_null
    (:if nil :i64 ((:local.get ,v)) ,(i64-decode-code `((:local.get ,b)) fail))))

(defun dual-lset-code (node effect)
  "Code for (:LSET var expr) when VAR is an integer accumulator and EXPR is
integer arithmetic (the fused trees of prims.lisp, here also a single
operation); NIL otherwise.  EFFECT: no value is left."
  (let* ((lv (second node)) (v (dual-local lv)) (expr (third node)))
    (when (and v (eq (car (get-loc lv)) :local) (not *no-fuse*) (fusable-op-node-p expr)
               (not (eq (fused-plan expr nil) :no)))
      (let ((b (cdr (get-loc lv))) (fail (gensym "SLOW")) (done (gensym "ACC")))
        (multiple-value-bind (leaves i64-code generic) (fused-parts expr)
          `(,@leaves
            (:block ,done nil
              (:block ,fail nil
                ,@(funcall i64-code fail)
                (:local.set ,v) (:ref.null :eq) (:local.set ,b)
                (:br ,done))
              ,@generic
              (:local.set ,b))
            ,@(unless effect (dual-materialize-code b v))))))))
