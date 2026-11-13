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
  ;; a reference representation: only variables are held in one
  (when (and (consp rep) (not (member (car node) '(:lref :lset :the))))
    (return-from cg-rep-1 nil))
  (case (car node)
    (:const (let ((v (second node)))
              (ecase rep
                (:f64 (when (typep v 'double-float) `((:f64.const ,v))))
                (:f32 (when (typep v 'single-float) `((:f32.const ,v))))
                (:c64 (when (and (complexp v) (typep (realpart v) 'double-float))
                        `((:f64.const ,(realpart v)) (:f64.const ,(imagpart v)))))
                (:c32 (when (and (complexp v) (typep (realpart v) 'single-float))
                        `((:f32.const ,(realpart v)) (:f32.const ,(imagpart v)))))
                (:i32 (when (typep v '(signed-byte 32)) `((:i32.const ,v))))
                (:i64 (when (typep v '(signed-byte 64)) `((:i64.const ,v)))))))
    (:lref (let ((lv (second node)))
             (when (unboxed-local-p lv)
               (let ((get `(:local.get ,(cdr (get-loc lv)))))
                 (cond ((eq (lvar-rep lv) rep) (rep-get-code rep (cdr (get-loc lv))))
                       ((and (eq (lvar-rep lv) :i64) (eq rep :i32)) (list get :i32.wrap_i64))
                       ((and (eq (lvar-rep lv) :i32) (eq rep :i64)) (list get :i64.extend_i32_s)))))))
    (:lset (let ((lv (second node)))
             (when (and (eq (lvar-rep lv) rep) (unboxed-local-p lv))
               `(,@(cg-rep (third node) rep) ,@(rep-local-set-code lv (cdr (get-loc lv)) t)))))
    (:the (cg-rep (third node) rep))
    (:progn (when (cdr node)
              `(,@(loop for n in (butlast (cdr node)) append (cg-effect n))
                ,@(cg-rep (car (last node)) rep))))
    (:if `(,@(cg-test (second node))
           (:if nil ,(rep-block-type rep) ,(cg-rep (third node) rep) ,(cg-rep (fourth node) rep))))
    (:let (cg-let node nil nil rep))
    ((:prim :call)
     (or (and (member (node-op-name node) '(clysm-sys::%aref1 svref row-major-aref clysm-sys::%aref2))
              (typed-elt-code node rep))
      (ecase rep
                     (:i64 (cond ((ty-int32-p (node-ty node)) `(,@(cg-rep node :i32) :i64.extend_i32_s))
                                 ((ty-int64-p (node-ty node)) (i64-op-code node))))
                     (:f64 (when (ty-double-p (node-ty node)) (f64-op-code node)))
                     (:f32 (when (ty-single-p (node-ty node)) (f32-op-code node)))
                     (:c64 (when (ty-cdouble-p (node-ty node)) (cplx-op-code node :f64)))
                     (:c32 (when (ty-csingle-p (node-ty node)) (cplx-op-code node :f32)))
                     (:i32 (when (ty-int32-p (node-ty node))
                             (or (i32-op-code node)
                                 ;; operands beyond 32 bits: in i64 (exact, as the result is within 32 bits)
                                 (let ((c (i64-op-code node))) (when c `(,@c :i32.wrap_i64)))
                                 (when (fused-tree-node-p node) (fused-code node :i32))))))))
    (t nil)))

;;; ------------------------------------------------------------
;;; f64
;;; ------------------------------------------------------------

(defun f64-arg-p (node &optional exact)
  "NODE can be converted to an f64 inline (EXACT: without rounding, as
comparisons need)."
  (let ((ty (node-ty node)))
    (or (ty-double-p ty) (ty-int32-p ty) (ty-single-p ty)
        (and (not exact) (ty-only-p ty +k-real+)))))

(defun f64-arg-code (node)
  (let ((ty (node-ty node)))
    (cond ((ty-double-p ty) (cg-rep node :f64))
          ((ty-single-p ty) `(,@(cg-rep node :f32) :f64.promote_f32))
          ((ty-int32-p ty) `(,@(cg-rep node :i32) :f64.convert_i32_s))
          (t `(,@(cg node) (:call :to-f64))))))

(defparameter *f64-generic*
  '((+ . clysm-sys::%generic-add) (- . clysm-sys::%generic-sub)
    (* . clysm-sys::%generic-mul) (/ . clysm-sys::%generic-div)))

(defun f64-binop-code (name a-code b-code)
  "A-CODE and B-CODE leave f64s.  A result that is infinite or NaN, or a zero
that may be an underflow, is redone by the generic function, which decides."
  (let* ((x (new-local :f64)) (y (new-local :f64)) (r (new-local :f64))
         (op (ecase name (+ :f64.add) (- :f64.sub) (* :f64.mul) (/ :f64.div)))
         (generic `((:local.get ,x) (:struct.new :double) (:local.get ,y) (:struct.new :double)
                   ,@(lisp-call-instrs (cdr (assoc name *f64-generic*)) 2)
                   (:ref.cast :double) (:struct.get :double 0))))
    `(,@a-code (:local.set ,x) ,@b-code (:local.set ,y)
      (:local.get ,x) (:local.get ,y) ,op (:local.set ,r)
      ,@(if (member name '(+ -))
            ;; not finite
            `((:local.get ,r) (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne
              (:hint 0) (:if nil :f64 ,generic ((:local.get ,r))))
            ;; one test of the exponent for zero, subnormal, infinite and NaN
            ;; results; then a zero result of nonzero operands may be an underflow
            `((:local.get ,r) :i64.reinterpret_f64 (:i64.const 52) :i64.shr_u :i32.wrap_i64
              (:i32.const #x7FF) :i32.and (:i32.const 1) :i32.sub (:i32.const #x7FE) :i32.ge_u
              (:hint 0)
              (:if nil :f64
                   ((:local.get ,r) (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne
                    (:local.get ,r) (:f64.const 0) :f64.eq (:local.get ,x) (:f64.const 0) :f64.ne :i32.and
                    ,@(when (eq name '*) `((:local.get ,y) (:f64.const 0) :f64.ne :i32.and))
                    :i32.or
                    (:if nil :f64 ,generic ((:local.get ,r))))
                   ((:local.get ,r))))))))

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
      (abs (cond ((ty-double-p (node-ty (first args)))
                  `(,@(cg-rep (first args) :f64) :f64.abs))
                 ((ty-cdouble-p (node-ty (first args))) (cplx-abs-code (first args) :f64))))
      ((realpart imagpart) (cplx-part-code name (first args) :f64))
      ((clysm-sys::%cd-real clysm-sys::%cd-imag)
       `(,@(cg (first args)) (:ref.cast :complex-d)
         (:struct.get :complex-d ,(if (eq name 'clysm-sys::%cd-real) 0 1))))
      (float (let ((x (first args)) (proto (second args)))
               (when (and (f64-arg-p x)
                          (or (and proto (ty-double-p (node-ty proto)))
                              (and (null proto) (ty-double-p (node-ty x)))))
                 (if (and proto (not (eq (car proto) :const)))
                     (let ((tmp (new-local :f64)))
                       `(,@(f64-arg-code x) (:local.set ,tmp) ,@(cg-effect proto) (:local.get ,tmp)))
                     (f64-arg-code x)))))
      (t nil))))

;;; ------------------------------------------------------------
;;; f32 (single floats)
;;; ------------------------------------------------------------

(defun f32-arg-p (node)
  "NODE is a single float, or an integer that the operation converts to one."
  (let ((ty (node-ty node)))
    (or (ty-single-p ty) (ty-int32-p ty))))

(defun f32-arg-code (node)
  (if (ty-single-p (node-ty node))
      (cg-rep node :f32)
      `(,@(cg-rep node :i32) :f32.convert_i32_s)))

(defun f32-binop-code (name a-code b-code)
  "A-CODE and B-CODE leave f32s; as F64-BINOP-CODE."
  (let* ((x (new-local :f32)) (y (new-local :f32)) (r (new-local :f32))
         (op (ecase name (+ :f32.add) (- :f32.sub) (* :f32.mul) (/ :f32.div)))
         (generic `((:local.get ,x) (:struct.new :single) (:local.get ,y) (:struct.new :single)
                    ,@(lisp-call-instrs (cdr (assoc name *f64-generic*)) 2)
                    (:ref.cast :single) (:struct.get :single 0))))
    `(,@a-code (:local.set ,x) ,@b-code (:local.set ,y)
      (:local.get ,x) (:local.get ,y) ,op (:local.set ,r)
      ,@(if (member name '(+ -))
            `((:local.get ,r) (:local.get ,r) :f32.sub (:f32.const 0.0) :f32.ne
              (:hint 0) (:if nil :f32 ,generic ((:local.get ,r))))
            `((:local.get ,r) :i32.reinterpret_f32 (:i32.const 23) :i32.shr_u
              (:i32.const #xFF) :i32.and (:i32.const 1) :i32.sub (:i32.const #xFE) :i32.ge_u
              (:hint 0)
              (:if nil :f32
                   ((:local.get ,r) (:local.get ,r) :f32.sub (:f32.const 0.0) :f32.ne
                    (:local.get ,r) (:f32.const 0.0) :f32.eq (:local.get ,x) (:f32.const 0.0) :f32.ne :i32.and
                    ,@(when (eq name '*) `((:local.get ,y) (:f32.const 0.0) :f32.ne :i32.and))
                    :i32.or
                    (:if nil :f32 ,generic ((:local.get ,r))))
                   ((:local.get ,r))))))))

(defun f32-op-code (node)
  "f32 code for an operation whose result is a single float, or NIL."
  (let ((name (node-op-name node)) (args (cddr node)))
    (case name
      ((+ - * /)
       (cond ((and (= (length args) 2) (every #'f32-arg-p args) (some (lambda (a) (ty-single-p (node-ty a))) args))
              (f32-binop-code name (f32-arg-code (first args)) (f32-arg-code (second args))))
             ((and (eq name '-) (= (length args) 1) (ty-single-p (node-ty (first args))))
              `(,@(cg-rep (first args) :f32) :f32.neg))))
      ((1+ 1-)
       (when (ty-single-p (node-ty (first args)))
         (f32-binop-code (if (eq name '1+) '+ '-) (cg-rep (first args) :f32) '((:f32.const 1.0)))))
      (abs (cond ((ty-single-p (node-ty (first args)))
                  `(,@(cg-rep (first args) :f32) :f32.abs))
                 ((ty-csingle-p (node-ty (first args))) (cplx-abs-code (first args) :f32))))
      ((realpart imagpart) (cplx-part-code name (first args) :f32))
      ((clysm-sys::%cs-real clysm-sys::%cs-imag)
       `(,@(cg (first args)) (:ref.cast :complex-s)
         (:struct.get :complex-s ,(if (eq name 'clysm-sys::%cs-real) 0 1))))
      (float (let ((x (first args)) (proto (second args)))
               (when (and proto (eq (car proto) :const) (ty-single-p (node-ty proto)) (ty-double-p (node-ty x)))
                 ;; a double rounded to a single; the generic function when it overflows or underflows
                 (let ((d (new-local :f64)) (r (new-local :f32)))
                   (return-from f32-op-code
                     `(,@(cg-rep x :f64) (:local.tee ,d) :f32.demote_f64 (:local.tee ,r)
                       :i32.reinterpret_f32 (:i32.const 23) :i32.shr_u
                       (:i32.const #xFF) :i32.and (:i32.const 1) :i32.sub (:i32.const #xFE) :i32.ge_u
                       (:local.get ,d) (:f64.const 0) :f64.ne :i32.and
                       (:hint 0)
                       (:if nil :f32
                            ((:local.get ,d) (:struct.new :double) ,@(cg proto) ,@(lisp-call-instrs 'float 2)
                             (:ref.cast :single) (:struct.get :single 0))
                            ((:local.get ,r)))))))
               (when (and (f32-arg-p x)
                          (or (null proto) (ty-single-p (node-ty proto))))
                 (if (and proto (not (eq (car proto) :const)))
                     (let ((tmp (new-local :f32)))
                       `(,@(f32-arg-code x) (:local.set ,tmp) ,@(cg-effect proto) (:local.get ,tmp)))
                     (f32-arg-code x)))))
      (t nil))))

;;; ------------------------------------------------------------
;;; complex numbers of floats: the two parts on the stack (W: :f64 or :f32)
;;; ------------------------------------------------------------

(defun fop (w op) (intern (concatenate 'string (symbol-name w) "." (symbol-name op)) :keyword))
(defun fzero (w) (if (eq w :f64) '(:f64.const 0d0) '(:f32.const 0.0)))
(defun cplx-rep (w) (if (eq w :f64) :c64 :c32))
(defun cplx-struct (w) (if (eq w :f64) :complex-d :complex-s))
(defun ty-cplx-p (ty w) (if (eq w :f64) (ty-cdouble-p ty) (ty-csingle-p ty)))
(defun float-arg-p (node w) (if (eq w :f64) (f64-arg-p node t) (f32-arg-p node)))
(defun float-arg-code (node w) (if (eq w :f64) (f64-arg-code node) (f32-arg-code node)))

(defun cplx-arg-p (node w)
  "NODE is a complex number of W floats, or a number that an operation with one converts exactly."
  (let ((ty (node-ty node)))
    (or (ty-cplx-p ty w) (float-arg-p node w) (and (eq w :f64) (ty-csingle-p ty)))))

(defun cplx-arg-code (node w)
  "The real and imaginary parts of NODE as W floats (the imaginary part of a
real number X is (* 0 X), as IMAGPART gives it)."
  (let ((ty (node-ty node)))
    (cond ((ty-cplx-p ty w) (cg-rep node (cplx-rep w)))
          ((and (eq w :f64) (ty-csingle-p ty))
           (let ((tmp (new-local :f32)))
             `(,@(cg-rep node :c32) (:local.set ,tmp) :f64.promote_f32 (:local.get ,tmp) :f64.promote_f32)))
          ((ty-only-p ty +k-int+) `(,@(float-arg-code node w) ,(fzero w)))
          (t (let ((tmp (new-local w)))
               `(,@(float-arg-code node w) (:local.tee ,tmp) ,(fzero w) (:local.get ,tmp) ,(fop w :mul)))))))

(defun cplx-binop-code (name w a-code b-code)
  "A-CODE and B-CODE leave the parts of two complex numbers; the sum,
difference or product inline, redone by the Lisp function for complex numbers
when a part of the result is not finite or a product may have underflowed."
  (let* ((ar (new-local w)) (ai (new-local w)) (br (new-local w)) (bi (new-local w))
         (re (new-local w)) (im (new-local w))
         (bt (rep-block-type (cplx-rep w))) (st (cplx-struct w))
         (generic `((:local.get ,ar) (:local.get ,ai) (:struct.new ,st)
                    (:local.get ,br) (:local.get ,bi) (:struct.new ,st)
                    ,@(lisp-call-instrs (ecase name (+ 'clysm-sys::%complex-add) (- 'clysm-sys::%complex-sub)
                                          (* 'clysm-sys::%complex-mul))
                                        2)
                    ,@(unbox-rep-code (cplx-rep w))))
         (result `((:local.get ,re) (:local.get ,im)))
         (not-finite `((:local.get ,re) (:local.get ,re) ,(fop w :sub) (:local.get ,im) (:local.get ,im) ,(fop w :sub)
                       ,(fop w :add) ,(fzero w) ,(fop w :ne))))
    `(,@a-code (:local.set ,ai) (:local.set ,ar) ,@b-code (:local.set ,bi) (:local.set ,br)
      ,@(if (member name '(+ -))
            (let ((op (fop w (if (eq name '+) :add :sub))))
              `((:local.get ,ar) (:local.get ,br) ,op (:local.set ,re)
                (:local.get ,ai) (:local.get ,bi) ,op (:local.set ,im)
                ,@not-finite (:hint 0) (:if nil ,bt ,generic ,result)))
            (let ((p1 (new-local w)) (p2 (new-local w)) (p3 (new-local w)) (p4 (new-local w)))
              (flet ((zerop-code (p) `((:local.get ,p) ,(fzero w) ,(fop w :eq)))
                     (underflow (p x y) `((:local.get ,p) ,(fzero w) ,(fop w :eq)
                                          (:local.get ,x) ,(fzero w) ,(fop w :ne) :i32.and
                                          (:local.get ,y) ,(fzero w) ,(fop w :ne) :i32.and)))
                `((:local.get ,ar) (:local.get ,br) ,(fop w :mul) (:local.set ,p1)
                  (:local.get ,ai) (:local.get ,bi) ,(fop w :mul) (:local.set ,p2)
                  (:local.get ,ar) (:local.get ,bi) ,(fop w :mul) (:local.set ,p3)
                  (:local.get ,ai) (:local.get ,br) ,(fop w :mul) (:local.set ,p4)
                  (:local.get ,p1) (:local.get ,p2) ,(fop w :sub) (:local.set ,re)
                  (:local.get ,p3) (:local.get ,p4) ,(fop w :add) (:local.set ,im)
                  ,@not-finite ,@(zerop-code p1) :i32.or ,@(zerop-code p2) :i32.or
                  ,@(zerop-code p3) :i32.or ,@(zerop-code p4) :i32.or
                  (:hint 0)
                  (:if nil ,bt
                       (,@not-finite ,@(underflow p1 ar br) :i32.or ,@(underflow p2 ai bi) :i32.or
                        ,@(underflow p3 ar bi) :i32.or ,@(underflow p4 ai br) :i32.or
                        (:if nil ,bt ,generic ,result))
                       ,result))))))))

(defun cplx-op-code (node w)
  "Code leaving the two parts of an operation whose result is a complex number of W floats, or NIL."
  (let ((name (node-op-name node)) (args (cddr node)) (rep (cplx-rep w)))
    (case name
      ((+ - *)
       (cond ((and (= (length args) 2) (every (lambda (a) (cplx-arg-p a w)) args))
              (cplx-binop-code name w (cplx-arg-code (first args) w) (cplx-arg-code (second args) w)))
             ((and (eq name '-) (= (length args) 1) (ty-cplx-p (node-ty (first args)) w))
              (let ((tmp (new-local w)))
                `(,@(cg-rep (first args) rep) (:local.set ,tmp) ,(fop w :neg) (:local.get ,tmp) ,(fop w :neg))))))
      (complex
       (flet ((part-p (a) (if (eq w :f64) (f64-arg-p a) (f32-arg-p a)))
              (part-code (a) (if (eq w :f64) (f64-arg-code a) (f32-arg-code a))))
         (when (and (<= 1 (length args) 2) (every #'part-p args))
           `(,@(part-code (first args)) ,@(if (second args) (part-code (second args)) (list (fzero w)))))))
      (conjugate
       (when (ty-cplx-p (node-ty (first args)) w)
         `(,@(cg-rep (first args) rep) ,(fop w :neg))))
      (t nil))))

(defun cplx-part-code (name arg w)
  "REALPART / IMAGPART of a complex number of W floats."
  (when (ty-cplx-p (node-ty arg) w)
    (if (eq name 'realpart)
        `(,@(cg-rep arg (cplx-rep w)) :drop)
        (let ((tmp (new-local w)))
          `(,@(cg-rep arg (cplx-rep w)) (:local.set ,tmp) :drop (:local.get ,tmp))))))

(defun cplx-abs-code (arg w)
  "ABS of a complex number of W floats (in f64; the Lisp function when the sum
of the squares is zero, subnormal or not finite, or the result not a single)."
  (let ((r (new-local :f64)) (i (new-local :f64)) (s (new-local :f64)) (r32 (new-local :f32))
        (tmp (new-local w)))
    `(,@(cg-rep arg (cplx-rep w)) (:local.set ,tmp)
      ,@(when (eq w :f32) '(:f64.promote_f32)) (:local.set ,r)
      (:local.get ,tmp) ,@(when (eq w :f32) '(:f64.promote_f32)) (:local.set ,i)
      (:local.get ,r) (:local.get ,r) :f64.mul (:local.get ,i) (:local.get ,i) :f64.mul :f64.add (:local.tee ,s)
      :i64.reinterpret_f64 (:i64.const 52) :i64.shr_u :i32.wrap_i64
      (:i32.const #x7FF) :i32.and (:i32.const 1) :i32.sub (:i32.const #x7FE) :i32.ge_u
      ,@(when (eq w :f32)
          `((:local.get ,s) :f64.sqrt :f32.demote_f64 (:local.tee ,r32)
            (:local.get ,r32) :f32.sub (:f32.const 0.0) :f32.ne :i32.or))
      (:hint 0)
      (:if nil ,w
           ((:local.get ,r) ,@(when (eq w :f32) '(:f32.demote_f64))
            (:local.get ,i) ,@(when (eq w :f32) '(:f32.demote_f64))
            (:struct.new ,(cplx-struct w))
            ,@(lisp-call-instrs 'clysm-sys::%complex-abs 1)
            ,@(unbox-rep-code w))
           ,(if (eq w :f32) `((:local.get ,r32)) `((:local.get ,s) :f64.sqrt))))))

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
                 ((ty-only-p ty +k-string+) `(,@(cg a) (:ref.cast! :string) :array.len))
                 ((and (eq name 'length) (ty-vec-kind ty))
                  `(,@(cg a) (:ref.cast ,(ty-vec-kind ty)) :array.len)))))
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
        (t nil)))))

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
        (t nil)))))

(defun typed-cg (node mv)
  "Code for a :PRIM or :CALL node using its types, or NIL."
  (let ((ty (node-ty node)) (name (node-op-name node)) (args (cddr node)))
    (when (and mv (member name '(floor truncate ceiling round)))
      (return-from typed-cg nil))
    (let ((code
            (cond ((and (eq (car node) :prim) (eq (prim-kind (second node)) :test))
                   (let ((c (typed-test-code node))) (when c `(,@c ,@(bool-code)))))
                  ((member name '(clysm-sys::%aref1 svref char schar row-major-aref clysm-sys::%aref2))
                   (typed-elt-code node :eqref))
                  ((member name '(clysm-sys::%aset1 clysm-sys::%svset clysm-sys::%row-major-aset
                                  clysm-sys::%aset2))
                   (typed-store-code node nil))
                  ((member name '(clysm-sys::%struct-ref clysm-sys::%struct-set))
                   (typed-struct-code node))
                  ((member name '(car cdr first rest clysm-sys::%lref clysm-sys::%lset
                                  clysm-sys::%svref clysm-sys::%set-svref))
                   (typed-access-code node))
                  ((and (eq name 'clysm-sys::%make-complex-d) (every (lambda (a) (ty-double-p (node-ty a))) args))
                   `(,@(cg-rep (first args) :f64) ,@(cg-rep (second args) :f64) (:struct.new :complex-d)))
                  ((and (eq name 'clysm-sys::%make-complex-s) (every (lambda (a) (ty-single-p (node-ty a))) args))
                   `(,@(cg-rep (first args) :f32) ,@(cg-rep (second args) :f32) (:struct.new :complex-s)))
                  ((ty-double-p ty)
                   (let ((c (f64-op-code node))) (when c `(,@c (:struct.new :double)))))
                  ((ty-single-p ty)
                   (let ((c (f32-op-code node))) (when c `(,@c (:struct.new :single)))))
                  ((ty-cdouble-p ty)
                   (let ((c (cplx-op-code node :f64))) (when c `(,@c (:struct.new :complex-d)))))
                  ((ty-csingle-p ty)
                   (let ((c (cplx-op-code node :f32))) (when c `(,@c (:struct.new :complex-s)))))
                  ((and (ty-int32-p ty) (or (i32-op-code node) (i64-op-code node)))
                   (let ((c (or (i32-op-code node) `(,@(i64-op-code node) :i32.wrap_i64))))
                     `(,@c ,@(box-rep-code :i32 ty))))
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

(defun storage-raw-rep-code (kind rep)
  "Instructions converting a raw element of a storage of KIND to REP, or :NO
when REP cannot hold every element."
  (destructuring-bind (name et wt lo hi) (storage-kind-info kind)
    (declare (ignore name et))
    (let ((raw (storage-raw-type kind)))
      (case rep
        (:eqref (storage-box-code kind))
        (:i32 (if (and (eq raw :i32) lo (>= lo (- (expt 2 31))) (< hi (expt 2 31))) nil :no))
        (:i64 (cond ((and (eq raw :i32) lo) (if (minusp lo) '(:i64.extend_i32_s) '(:i64.extend_i32_u)))
                    ((member kind '(:s64vec :u63vec)) nil)
                    (t :no)))
        (:f64 (if (eq wt :f64) nil :no))
        (:f32 (if (eq wt :f32) nil :no))
        (t :no)))))

(defun array-locate-code (name args slow)
  "For an element access NAME of ARGS (the array and its subscripts, the nodes):
returns (values setup locate kind data index slow-args) or NIL when the types
are not known.  SETUP evaluates the arguments into locals; LOCATE branches to
SLOW unless the array is a simple array over a storage of KIND with the
subscripts within its dimensions, and otherwise leaves the storage in the
local DATA (a reference to the storage type of KIND) and the row-major index
in the local INDEX; SLOW-ARGS pushes the array and the subscripts for the
generic function."
  (let* ((a (first args)) (aty (node-ty a)) (subs (rest args))
         (two (member name '(clysm-sys::%aref2 clysm-sys::%aset2)))
         (row-major (member name '(row-major-aref clysm-sys::%row-major-aset))))
    (multiple-value-bind (kind shape) (ty-array-kind aty)
      (when (and kind (every #'fx-arg-p subs)
                 (or (not (member name '(char schar))) (eq kind :string))
                 (cond (two (member shape '(:md2 :any)))
                       (row-major t)
                       (t (member shape '(:vec :any)))))
        (let ((e (new-local)) (d (new-local `(:ref :null ,kind))) (k (new-local :i32)))
          (cond
            ((and (not two) (eq shape :vec) (eq (lvar-rep-of a) (ref-rep kind)))
             ;; a variable held as a reference to the storage: no test
             (values `(,@(cg-rep a (ref-rep kind)) (:local.set ,d) ,@(cg-rep (first subs) :i32) (:local.set ,k))
                     `((:local.get ,k) (:local.get ,d) :array.len :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow))
                     kind d k
                     `((:local.get ,d) (:local.get ,k) :ref.i31)))
            ((and (eq shape :md2) (eq (lvar-rep-of a) (md2-rep kind)))
             ;; a variable held with its storage, dimensions and offset in locals
             (destructuring-bind (data d0 d1 off) (md2-shadows (second a))
               (let ((x (new-local :i32)) (y (new-local :i32)))
                 (values
                  `(,@(cg-rep (first subs) :i32) (:local.set ,x)
                    ,@(when two `(,@(cg-rep (second subs) :i32) (:local.set ,y))))
                  `(,@(if two
                          `((:local.get ,x) (:local.get ,d0) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,y) (:local.get ,d1) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,off) (:local.get ,x) (:local.get ,d1) :i32.mul :i32.add (:local.get ,y) :i32.add)
                          `((:local.get ,x) (:local.get ,d0) (:local.get ,d1) :i32.mul :i32.lt_u :i32.eqz
                            (:hint 0) (:br_if ,slow)
                            (:local.get ,off) (:local.get ,x) :i32.add))
                    (:local.set ,k) (:local.get ,data) (:local.set ,d))
                  kind d k
                  `(,@(cg a) (:local.get ,x) :ref.i31 ,@(when two `((:local.get ,y) :ref.i31)))))))
            ((and (not two) (member shape '(:vec :any)) (not (and row-major (eq shape :any))))
              ;; the array is the storage
              (values `(,@(cg a) (:local.set ,e) ,@(cg-rep (first subs) :i32) (:local.set ,k))
                      `((:local.get ,e) (:ref.test ,kind) :i32.eqz (:hint 0) (:br_if ,slow)
                        (:local.get ,e) (:ref.cast! ,kind) (:local.set ,d)
                        (:local.get ,k) (:local.get ,d) :array.len :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow))
                      kind d k
                      `((:local.get ,e) (:local.get ,k) :ref.i31)))
            (t
              ;; a header (with ROW-MAJOR-AREF of an array of any rank: only two-dimensional ones inline)
              (let ((m (new-local '(:ref :null :mdarray))) (s (new-local))
                    (x (new-local :i32)) (y (new-local :i32)) (d1 (new-local :i32)))
                (values
                 `(,@(cg a) (:local.set ,e)
                   ,@(cg-rep (first subs) :i32) (:local.set ,x)
                   ,@(when two `(,@(cg-rep (second subs) :i32) (:local.set ,y))))
                 `((:local.get ,e) (:ref.test :mdarray) :i32.eqz (:br_if ,slow)
                   (:local.get ,e) (:ref.cast! :mdarray) (:local.set ,m)
                   ,@(if two
                         `((:local.get ,x) (:local.get ,m) (:struct.get :mdarray 5) :i32.ge_u (:br_if ,slow)
                           (:local.get ,y) (:local.get ,m) (:struct.get :mdarray 6) (:local.tee ,d1) :i32.ge_u
                           (:br_if ,slow))
                         `((:local.get ,x) (:local.get ,m) (:struct.get :mdarray 5)
                           (:local.get ,m) (:struct.get :mdarray 6) :i32.mul :i32.ge_u (:br_if ,slow)))
                   (:local.get ,m) (:struct.get :mdarray 0) (:local.tee ,s) (:ref.test ,kind) :i32.eqz (:br_if ,slow)
                   (:local.get ,s) (:ref.cast! ,kind) (:local.set ,d)
                   (:local.get ,m) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s
                   ,@(if two
                         `((:local.get ,x) (:local.get ,d1) :i32.mul :i32.add (:local.get ,y) :i32.add)
                         `((:local.get ,x) :i32.add))
                   (:local.set ,k))
                 kind d k
                 `((:local.get ,e) (:local.get ,x) :ref.i31 ,@(when two `((:local.get ,y) :ref.i31))))))))))))

(defun array-slow-returns-p (name args)
  "The generic function may return when the inline access does not apply:
ROW-MAJOR-AREF of an array of unknown rank (inline for two dimensions only)."
  (and (member name '(row-major-aref clysm-sys::%row-major-aset))
       (eq (nth-value 1 (ty-array-kind (node-ty (first args)))) :any)))

(defun array-slow-call (name)
  (ecase name
    ((clysm-sys::%aref1 svref char schar) '((:call :aref1)))
    ((clysm-sys::%aset1 clysm-sys::%svset) '((:call :aset1)))
    (clysm-sys::%aref2 '((:call :aref2)))
    (clysm-sys::%aset2 '((:call :aset2)))
    (row-major-aref (lisp-call-instrs 'row-major-aref 2))
    (clysm-sys::%row-major-aset (lisp-call-instrs 'clysm-sys::%row-major-aset 3))))

(defun typed-elt-code (node rep)
  "An element read (AREF of one or two subscripts, SVREF, CHAR, SCHAR,
ROW-MAJOR-AREF) of a simple array whose storage kind is known, with fixnum
subscripts: the element is read inline when the subscripts are within the
bounds (else the generic function, which signals the error) and left in REP.
NIL when the types are not known or REP cannot hold the elements."
  (let ((name (node-op-name node)) (slow (gensym "SLOW")) (done (gensym "ELT")))
    (multiple-value-bind (setup locate kind d k slow-args) (array-locate-code name (cddr node) slow)
      (when setup
        (let ((conv (storage-raw-rep-code kind rep)))
          (unless (eq conv :no)
            `(,@setup
              (:block ,done ,(rep-wasm-type rep)
                (:block ,slow nil
                  ,@locate
                  (:local.get ,d) (:local.get ,k) ,@(storage-get-code kind) ,@conv
                  (:br ,done))
                ;; (the generic function signals the error: a simple array
                ;; of this kind with the subscripts in bounds is read above)
                ,@slow-args ,@(array-slow-call name)
                ,@(if (array-slow-returns-p name (cddr node))
                      (unbox-rep-code rep (node-ty node))
                      '(:unreachable))))))))))

(defun typed-store-code (node effect)
  "An element store ((SETF AREF) of one or two subscripts, (SETF SVREF),
(SETF ROW-MAJOR-AREF)) into a simple array whose storage kind is known, with
fixnum subscripts: inline when the subscripts are within the bounds and the
value is of the element type (else the generic function).  With EFFECT the
code leaves nothing, else the value."
  (let* ((name (node-op-name node)) (slow (gensym "SLOW")) (done (gensym "STORE"))
         (val (car (last (cddr node)))) (vty (node-ty val)))
    (multiple-value-bind (setup locate kind d k slow-args) (array-locate-code name (butlast (cddr node)) slow)
      (when setup
        (destructuring-bind (kname et wt lo hi) (storage-kind-info kind)
          (declare (ignore kname et wt))
          (let* ((raw-type (storage-raw-type kind))
                 (raw (new-local raw-type))
                 ;; the value's type proves it is an element: no test, no box
                 (proven (case kind
                           (:vector :eqref)
                           (:string nil)
                           (:f32vec (and (ty-single-p vty) :f32))
                           (:f64vec (and (ty-double-p vty) :f64))
                           ((:u64vec :u63vec) (and (ty-in-range-p vty 0 (1- (expt 2 63))) :i64))
                           ((:s64vec) (and (ty-int64-p vty) :i64))
                           ((:u32vec) (and (ty-in-range-p vty lo hi) :i64))
                           (t (and (ty-in-range-p vty lo hi) :i32))))
                 (x (unless proven (new-local))))
            (when (or proven (member kind '(:string :bits :u7vec :bytes :s8vec :u15vec :u16vec :s16vec :f32vec :f64vec)))
              `(,@setup
                ,@(if proven
                      `(,@(cg-rep val proven) ,@(when (eq kind :u32vec) '(:i32.wrap_i64)) (:local.set ,raw))
                      `(,@(cg val) (:local.set ,x)))
                (:block ,done nil
                  (:block ,slow nil
                    ,@locate
                    ,@(unless proven (storage-unbox-code kind x raw `((:br ,slow))))
                    (:local.get ,d) (:local.get ,k) (:local.get ,raw) (:array.set ,kind)
                    (:br ,done))
                  ,@slow-args
                  ,@(if proven `((:local.get ,raw) ,@(storage-box-code kind)) `((:local.get ,x)))
                  ,@(array-slow-call name)
                  ,@(if (array-slow-returns-p name (cddr node)) '(:drop) '(:unreachable)))
                ,@(unless effect
                    (if proven `((:local.get ,raw) ,@(storage-box-code kind)) `((:local.get ,x))))))))))))

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
                     (or (ty-double-p (node-ty a)) (ty-double-p (node-ty b))
                         (ty-single-p (node-ty a)) (ty-single-p (node-ty b))))
                `(,@(f64-arg-code a) ,@(f64-arg-code b)
                  ,(ecase name (< :f64.lt) (> :f64.gt) (<= :f64.le) (>= :f64.ge) (= :f64.eq) (/= :f64.ne)))))))
      ((zerop plusp minusp)
       (cond ((i32-arg-p a) `(,@(cg-rep a :i32) ,@(ecase name (zerop '(:i32.eqz))
                                                  (plusp '((:i32.const 0) :i32.gt_s))
                                                  (minusp '((:i32.const 0) :i32.lt_s)))))
             ((ty-double-p (node-ty a))
              `(,@(cg-rep a :f64) (:f64.const 0) ,(ecase name (zerop :f64.eq) (plusp :f64.gt) (minusp :f64.lt))))
             ((ty-single-p (node-ty a))
              `(,@(cg-rep a :f32) (:f32.const 0.0) ,(ecase name (zerop :f32.eq) (plusp :f32.gt) (minusp :f32.lt))))))
      ((evenp oddp)
       (when (i32-arg-p a)
         `(,@(cg-rep a :i32) (:i32.const 1) :i32.and ,@(when (eq name 'evenp) '(:i32.eqz)))))
      ((eq eql)
       (when (and (= (length args) 2) (fx-arg-p a) (fx-arg-p b)
                  (or (eq (lvar-rep-of a) :i32) (eq (lvar-rep-of b) :i32)))
         `(,@(cg-rep a :i32) ,@(cg-rep b :i32) :i32.eq)))
      (t nil))))

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
