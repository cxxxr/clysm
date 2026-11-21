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

(defun trusted-fixnum-code (node)
  "An i32 of the value of NODE, declared a fixnum at (SAFETY 0) (THE) but not
known to be a 32-bit integer: arithmetic within 64 bits is wrapped to 32
bits; any other value is an i31 (no test of overflow or of the type)."
  (let ((ty (node-ty node)))
    (if (and (ty-int64-p ty) (member (car node) '(:prim :call))
             (member (node-op-name node) *i64-arith-ops*))
        `(,@(cg-rep node :i64) :i32.wrap_i64)
        (or (and (eq (car node) :call) (raw-call-rep-code node :i32))
            `(,@(cg node) (:ref.cast! :i31) :i31.get_s)))))

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
             ;; an integer accumulator read as an integer: its i64 without making the integer
             (when (and (member rep '(:i64 :i32)) (dual-local lv) (eq (car (get-loc lv)) :local)
                        (if (eq rep :i64) (ty-int64-p (node-ty node)) (ty-int32-p (node-ty node))))
               (let ((b (cdr (get-loc lv))))
                 (return-from cg-rep-1
                   `((:local.get ,b) :ref.is_null
                     (:if nil :i64 ((:local.get ,(dual-local lv))) ((:local.get ,b) (:call :int-i64)))
                     ,@(when (eq rep :i32) '(:i32.wrap_i64))))))
             (when (unboxed-local-p lv)
               (let ((get `(:local.get ,(cdr (get-loc lv)))))
                 (cond ((eq (lvar-rep lv) rep) (rep-get-code rep (cdr (get-loc lv))))
                       ((and (eq (lvar-rep lv) :i64) (eq rep :i32)) (list get :i32.wrap_i64))
                       ((and (eq (lvar-rep lv) :i32) (eq rep :i64)) (list get :i64.extend_i32_s)))))))
    (:lset (let ((lv (second node)))
             (when (and (eq (lvar-rep lv) rep) (unboxed-local-p lv))
               `(,@(cg-rep (third node) rep) ,@(rep-local-set-code lv (cdr (get-loc lv)) t)))))
    (:the (if (and (eq rep :i32) (eq (fourth node) :trust) (ty-fixnum-p (node-ty node))
                   (not (ty-int32-p (node-ty (third node)))))
              (trusted-fixnum-code (third node))
              (or (cg-rep-1 (third node) rep)
                  ;; (THE FIXNUM x) of a value of unknown type (read from
                  ;; data): its i31 inline, the general conversion otherwise
                  (if (and (member rep '(:i32 :i64)) (ty-fixnum-p (node-ty node))
                           (not (ty-int32-p (node-ty (third node)))))
                      (let ((tmp (new-local)))
                        `(,@(cg (third node)) (:local.tee ,tmp) (:ref.test :i31)
                          (:if nil ,rep
                               ((:local.get ,tmp) (:ref.cast! :i31) :i31.get_s
                                ,@(when (eq rep :i64) '(:i64.extend_i32_s)))
                               ((:local.get ,tmp) (:call :int-i64) ,@(when (eq rep :i32) '(:i32.wrap_i64))))))
                      `(,@(cg (third node)) ,@(unbox-rep-code rep (node-ty (third node))))))))
    (:progn (when (cdr node)
              `(,@(loop for n in (butlast (cdr node)) append (cg-effect n))
                ,@(cg-rep (car (last node)) rep))))
    (:if (if (assumed-guard-p (second node))
             (cg-rep (third node) rep)
         `(,@(cg-test (second node)) ,@(if-branch-hint (third node) (fourth node))
           (:if nil ,(rep-block-type rep) ,(cg-rep (third node) rep)
                (,@(cg-rep (fourth node) rep) ,@(guard-else-epilogue (second node)))))))
    (:let (cg-let node nil nil rep))
    ;; a block that nothing returns from (the body of a copied function)
    (:block (when (not (blk-nonlocal (second node)))
              (cond ((not (block-returned-to-p (second node) (third node)))
                     (cg-rep (third node) rep))
                    ;; a block returned from in the function: raw, as its
                    ;; RETURN-FROMs' values (babel's decoders' SETTER-BLOCK)
                    ((case rep
                       (:i32 (ty-int32-p (node-ty node)))
                       (:i64 (ty-int64-p (node-ty node)))
                       (:f64 (ty-double-p (node-ty node))))
                     (cg-block node nil nil rep)))))
    ((:prim :call)
     (or (and (eq (car node) :call) (member rep '(:i32 :i64 :f64))
              (raw-call-rep-code node rep))
         (and (member (node-op-name node) '(clysm-sys::%aref1 svref row-major-aref clysm-sys::%aref2))
              (typed-elt-code node rep))
         (and (eq (node-op-name node) 'clysm-sys::%struct-tref) (eq rep :f64)
              (tstruct-ref-code node :f64))
      (ecase rep
                     (:i64 (cond ((ty-int32-p (node-ty node)) `(,@(cg-rep node :i32) :i64.extend_i32_s))
                                 ((ty-int64-p (node-ty node))
                                  (or (i64-op-code node) (fused-i64-rep-code node)))))
                     (:f64 (if (ty-double-p (node-ty node))
                               (f64-op-code node)
                               ;; arithmetic that may give a double, taken as
                               ;; one (a variable declared DOUBLE-FLOAT): its
                               ;; fast path unboxed, else the object's double
                               (when (and (eq (car node) :prim) (spec-tree-node-p node t))
                                 (let ((b (new-local)) (f (new-local :f64)))
                                   `(,@(spec-tree-code node (cons b f))
                                     (:local.get ,b) :ref.is_null
                                     (:if nil :f64 ((:local.get ,f))
                                          ((:local.get ,b) ,@(unbox-rep-code :f64 (node-ty node)))))))))
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
          ;; (rounded to the nearest double, as FLOAT does)
          ((ty-int64-p ty) `(,@(cg-rep node :i64) :f64.convert_i64_s))
          (t `(,@(cg node) (:call :to-f64))))))

(defparameter *f64-generic*
  '((+ . clysm-sys::%generic-add) (- . clysm-sys::%generic-sub)
    (* . clysm-sys::%generic-mul) (/ . clysm-sys::%generic-div)))

(defun f64-binop-code (name a-code b-code)
  "A-CODE and B-CODE leave f64s.  A result that is infinite or NaN (an
overflow, a division by zero) is redone by the generic function, which
decides; an underflow is no condition (SBCL's default traps).
At (SAFETY 0) there are no such tests: the IEEE result."
  (when (eql (cg-safety) 0)
    (return-from f64-binop-code
      `(,@a-code ,@b-code ,(ecase name (+ :f64.add) (- :f64.sub) (* :f64.mul) (/ :f64.div)))))
  (let* ((x (new-local :f64)) (y (new-local :f64)) (r (new-local :f64))
         (op (ecase name (+ :f64.add) (- :f64.sub) (* :f64.mul) (/ :f64.div)))
         (generic `((:local.get ,x) (:struct.new :double) (:local.get ,y) (:struct.new :double)
                   ,@(lisp-call-instrs (cdr (assoc name *f64-generic*)) 2)
                   (:ref.cast :double) (:struct.get :double 0))))
    `(,@a-code (:local.set ,x) ,@b-code (:local.set ,y)
      (:local.get ,x) (:local.get ,y) ,op (:local.set ,r)
      ;; not finite (an underflow is not signalled: see %FLOAT-RESULT)
      ,@(f64-not-finite-code name r)
      (:hint 0) (:if nil :f64 ,generic ((:local.get ,r))))))

(defun f64-not-finite-code (name r)
  "An i32: the f64 in local R is an infinity or a NaN.  For a product or a
quotient the test of the exponent, which measured faster than the
subtraction (whose comparison V8 makes two branches) where the products are
a recurrence (mandelbrot at (SAFETY 1): 235 ms against 312)."
  (if (member name '(+ -))
      `((:local.get ,r) (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne)
      `((:local.get ,r) :i64.reinterpret_f64 (:i64.const 52) :i64.shr_u :i32.wrap_i64
        (:i32.const #x7FF) :i32.and (:i32.const #x7FF) :i32.eq)))

(defun f64-op-code (node)
  "f64 code for an operation whose result is a double, or NIL."
  (let ((name (node-op-name node)) (args (cddr node)))
    (or
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
      (t nil))
    ;; the numeric library (below)
    (numeric-f64-op-code node))))

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
  (when (eql (cg-safety) 0)
    (return-from f32-binop-code
      `(,@a-code ,@b-code ,(ecase name (+ :f32.add) (- :f32.sub) (* :f32.mul) (/ :f32.div)))))
  (let* ((x (new-local :f32)) (y (new-local :f32)) (r (new-local :f32))
         (op (ecase name (+ :f32.add) (- :f32.sub) (* :f32.mul) (/ :f32.div)))
         (generic `((:local.get ,x) (:struct.new :single) (:local.get ,y) (:struct.new :single)
                    ,@(lisp-call-instrs (cdr (assoc name *f64-generic*)) 2)
                    (:ref.cast :single) (:struct.get :single 0))))
    `(,@a-code (:local.set ,x) ,@b-code (:local.set ,y)
      (:local.get ,x) (:local.get ,y) ,op (:local.set ,r)
      (:local.get ,r) (:local.get ,r) :f32.sub (:f32.const 0.0) :f32.ne
      (:hint 0) (:if nil :f32 ,generic ((:local.get ,r))))))

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
                 ;; a double rounded to a single; the generic function when it overflows
                 (let ((d (new-local :f64)) (r (new-local :f32)))
                   (return-from f32-op-code
                     `(,@(cg-rep x :f64) (:local.tee ,d) :f32.demote_f64 (:local.tee ,r)
                       (:local.get ,r) :f32.sub (:f32.const 0.0) :f32.ne
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
when a part of the result is not finite."
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
            ;; (a product of infinite parts is not finite either: only a
            ;; NaN part can hide an infinity, and it is not finite)
            `((:local.get ,ar) (:local.get ,br) ,(fop w :mul) (:local.get ,ai) (:local.get ,bi) ,(fop w :mul)
              ,(fop w :sub) (:local.set ,re)
              (:local.get ,ar) (:local.get ,bi) ,(fop w :mul) (:local.get ,ai) (:local.get ,br) ,(fop w :mul)
              ,(fop w :add) (:local.set ,im)
              ,@not-finite (:hint 0) (:if nil ,bt ,generic ,result))))))

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
      (or
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
         (or (char-code-elt-code a)
             (when (ty-only-p (node-ty a) +k-char+)
               `(,@(cg a) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.sub))))
        (clysm-sys::%lref (when (ty-only-p (node-ty a) +k-limbs+)
                            `(,@(cg a) (:ref.cast! :limbs) ,@(index-i32-code b) (:array.get :limbs))))
        (clysm-sys::%llen (when (ty-only-p (node-ty a) +k-limbs+) `(,@(cg a) (:ref.cast! :limbs) :array.len)))
        ((length clysm-sys::%svlength clysm-sys::%string-length)
         (let ((ty (node-ty a)))
           (cond ((ty-only-p ty +k-svec+) `(,@(cg a) (:ref.cast! :vector) :array.len))
                 ((ty-only-p ty +k-string+) `(,@(cg a) (:ref.cast! :string) :array.len))
                 ((and (eq name 'length) (ty-vec-kind ty))
                  `(,@(cg a) (:ref.cast ,(ty-vec-kind ty)) :array.len))
                 ((and (eq name 'length) (not (ty-only-p ty +k-list+)))
                  ;; of unknown type: a simple vector or string, or a vector
                  ;; with a fill pointer, inline; else the function
                  (let ((e (new-local)) (f (new-local)) (done (gensym "LEN")))
                    `(,@(cg a) (:local.set ,e)
                      (:block ,done :i32
                        (:local.get ,e) (:ref.test :vector)
                        (:if nil nil ((:local.get ,e) (:ref.cast! :vector) :array.len (:br ,done)))
                        (:local.get ,e) (:ref.test :string)
                        (:if nil nil ((:local.get ,e) (:ref.cast! :string) :array.len (:br ,done)))
                        (:local.get ,e) (:ref.test :mdarray)
                        (:if nil nil ((:local.get ,e) (:ref.cast! :mdarray) (:struct.get :mdarray 1) (:local.tee ,f)
                                      (:ref.test :i31)
                                      (:if nil nil ((:local.get ,f) (:ref.cast! :i31) :i31.get_s (:br ,done)))))
                        (:local.get ,e) ,@(lisp-call-instrs 'length 1) (:ref.cast :i31) :i31.get_s)))))))
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
        (t nil))
      ;; the numeric library (below)
      (numeric-i32-op-code node)))))

;;; ------------------------------------------------------------
;;; eqref results and tests
;;; ------------------------------------------------------------

(defun i64-arg-p (node) (ty-int64-p (node-ty node)))

(defun exact-int-division-code (node)
  "(/ a k) of an integer A within 64 bits by a constant integer K (not 0 or
-1): the quotient when K divides A, else the generic division (a ratio)."
  (when (and (eq (node-op-name node) '/) (= (length (cddr node)) 2))
    (destructuring-bind (a b) (cddr node)
      (let ((k (const-int-node b)))
        (when (and k (typep k '(signed-byte 32)) (not (member k '(0 -1))) (i64-arg-p a))
          (let ((x (new-local :i64)) (q (new-local :i64)))
            `(,@(cg-rep a :i64) (:local.tee ,x) ,@(cg-effect b) (:i64.const ,k) :i64.rem_s :i64.eqz
              (:if nil :eqref
                   ((:local.get ,x) (:i64.const ,k) :i64.div_s (:local.tee ,q)
                    (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                    (:if nil :eqref ((:local.get ,q) :i32.wrap_i64 :ref.i31) ((:local.get ,q) (:call :i64-int))))
                   ((:local.get ,x) (:call :i64-int) (:i64.const ,k) (:call :i64-int) (:call :div))))))))))

(defun i64-op-code (node)
  "i64 code for an integer operation whose result is within 64 bits, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (flet ((both (op) (when (and (= (length args) 2) (i64-arg-p a) (i64-arg-p b))
                        `(,@(cg-rep a :i64) ,@(cg-rep b :i64) ,op))))
      (or
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
               (cond ((and c (i64-arg-p a) (<= -64 c 63))
                      (if (>= c 0)
                          `(,@(cg-rep a :i64) ,@(cg-effect b) (:i64.const ,c) :i64.shl)
                          `(,@(cg-rep a :i64) ,@(cg-effect b) (:i64.const ,(min 63 (- c))) :i64.shr_s)))
                     ;; a count of a known range (the result is within 64
                     ;; bits: a left shift loses no bit); a right shift by 63
                     ;; at most (Wasm takes the count modulo 64)
                     ((and b (i64-arg-p a) (ty-int32-p (node-ty b)))
                      (let ((lo (ty-lo (node-ty b))) (hi (ty-hi (node-ty b)))
                            (x (new-local :i64)) (n (new-local :i32)))
                        (flet ((right () `((:local.get ,x) (:i32.const 0) (:local.get ,n) :i32.sub (:local.tee ,n)
                                           (:i32.const 63) (:local.get ,n) (:i32.const 63) :i32.lt_u :select
                                           :i64.extend_i32_u :i64.shr_s))
                               (left () `((:local.get ,x) (:local.get ,n) :i64.extend_i32_u :i64.shl)))
                          (let ((shift (cond ((and lo (>= lo 0) hi (<= hi 63)) (left))
                                             ((and hi (<= hi 0)) (right))
                                             ((and hi (<= hi 63))
                                              `((:local.get ,n) (:i32.const 0) :i32.ge_s
                                                (:if nil :i64 ,(left) ,(right)))))))
                            (when shift
                              `(,@(cg-rep a :i64) (:local.set ,x) ,@(cg-rep b :i32) (:local.set ,n) ,@shift)))))))))
        (t nil))
      ;; the numeric library (below)
      (numeric-i64-op-code node)))))

(defun typed-cg (node mv)
  "Code for a :PRIM or :CALL node using its types, or NIL."
  (let ((ty (node-ty node)) (name (node-op-name node)) (args (cddr node)))
    (when (and mv (member name '(floor truncate ceiling round)))
      (return-from typed-cg nil))
    (when (and mv (member name '(ffloor fceiling ftruncate fround)))
      ;; both values: the remainder in the buffer of values
      (return-from typed-cg
        (when (float-divmod-node-p node)
          (multiple-value-bind (code r) (typed-float-divmod node :fquotient t)
            `(,@code (:struct.new :double)
              (:global.get :mv-buf) (:i32.const 0) (:local.get ,r) (:struct.new :double) (:array.set :vector)
              (:i32.const 2) (:global.set :mv-count))))))
    (let ((code
            (cond ((and (eq (car node) :prim) (eq (prim-kind (second node)) :test))
                   (let ((c (typed-test-code node))) (when c `(,@c ,@(bool-code)))))
                  ((numeric-typed-code node mv))
                  ((member name '(clysm-sys::%aref1 svref char schar row-major-aref clysm-sys::%aref2))
                   (typed-elt-code node :eqref))
                  ((member name '(clysm-sys::%aset1 clysm-sys::%svset clysm-sys::%row-major-aset
                                  clysm-sys::%aset2 clysm-sys::%charset clysm-sys::%scharset))
                   (typed-store-code node nil))
                  ((eq name 'clysm-sys::%replace) (replace-copy-code node))
                  ;; CODE-CHAR of a fixnum: the character from its i32 (no box)
                  ((and (eq name 'code-char) (= (length args) 1) (fx-arg-p (first args)))
                   `(,@(cg-rep (first args) :i32) (:i32.const ,+char-tag+) :i32.add :ref.i31))
                  ((member name '(clysm-sys::%struct-ref clysm-sys::%struct-set))
                   (typed-struct-code node))
                  ((eq name 'clysm-sys::%struct-tref) (tstruct-ref-code node :eqref))
                  ((eq name 'clysm-sys::%struct-tset) (tstruct-set-code node nil))
                  ((member name '(clysm-sys::%make-tstruct clysm-sys::%make-tstruct-default clysm-sys::%copy-tstruct))
                   (tstruct-make-code node))
                  ((member name '(clysm-sys::%struct-iref clysm-sys::%struct-iset))
                   (or (typed-struct-code node)
                       ;; a constant slot index: the field read inline
                       (multiple-value-bind (x cls i v size vp) (struct-iref-args node)
                         (let ((k (const-int-node i)) (n (const-int-node size)))
                           (when (and k n (< -1 k (mod n 64)) (<= (mod n 64) +struct-inline-max+))
                             (if vp
                                 (struct-iref-code (cg x) (cg cls) k (mod n 64) (floor n 64) (cg v))
                                 (struct-iref-code (cg x) (cg cls) k (mod n 64) (floor n 64))))))))
                  ((member name '(car cdr first rest clysm-sys::%lref clysm-sys::%lset
                                  clysm-sys::%svref clysm-sys::%set-svref
                                  clysm-sys::%setcar clysm-sys::%setcdr rplaca rplacd))
                   (typed-access-code node))
                  ((and (eq name 'clysm-sys::%make-complex-d) (every (lambda (a) (ty-double-p (node-ty a))) args))
                   `(,@(cg-rep (first args) :f64) ,@(cg-rep (second args) :f64) (:struct.new :complex-d)))
                  ((and (eq name 'clysm-sys::%make-complex-s) (every (lambda (a) (ty-single-p (node-ty a))) args))
                   `(,@(cg-rep (first args) :f32) ,@(cg-rep (second args) :f32) (:struct.new :complex-s)))
                  ((ty-double-p ty)
                   (let ((c (f64-op-code node))) (when c `(,@c (:struct.new :double)))))
                  ((exact-int-division-code node))
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

(defun subscript-box-code (node local)
  "The subscript NODE, held as an i32 in LOCAL, as a Lisp integer (for the
generic function, which signals the error of a subscript out of bounds)."
  (if (fx-arg-p node)
      `((:local.get ,local) :ref.i31)
      `((:local.get ,local) (:call :box-i32))))

(defun subscript-i32-parts (node x slow)
  "For the subscript NODE, to be held as an i32 in the local X: (values setup
locate slow-arg), the code evaluating it, the code leaving it in X (branching
to SLOW unless it is a fixnum when its type is not known: a character or a
negative fixnum is beyond any dimension compared unsigned) and the code
pushing it for the generic function."
  (if (i32-arg-p node)
      (values `(,@(cg-rep node :i32) (:local.set ,x)) nil (subscript-box-code node x))
      (let ((e (new-local)))
        (values `(,@(cg node) (:local.set ,e))
                `((:local.get ,e) (:ref.test :i31) :i32.eqz (:hint 0) (:br_if ,slow)
                  (:local.get ,e) (:ref.cast! :i31) :i31.get_s (:local.set ,x))
                `((:local.get ,e))))))

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
    (unless (or (ty-array-kind aty) two)
      (return-from array-locate-code (speculative-locate-code name a (first subs) slow)))
    ;; a special variable's array kept in locals on entry to the loop (SREF-MD2-WRAP)
    (let ((cache (and two (not (ty-array-kind aty)) (sref-md2-cache-of a))))
      (when cache
        (destructuring-bind (c data d0 d1 off) (cdr cache)
          (let ((e (new-local)) (d (new-local '(:ref :null :vector))) (k (new-local :i32))
                (x (new-local :i32)) (y (new-local :i32)))
            (multiple-value-bind (xs xl xb) (subscript-i32-parts (first subs) x slow)
              (multiple-value-bind (ys yl yb) (subscript-i32-parts (second subs) y slow)
                (return-from array-locate-code
                  (values `(,@(cg a) (:local.set ,e) ,@xs ,@ys)
                          `((:local.get ,e) (:local.get ,c) :ref.eq :i32.eqz (:hint 0) (:br_if ,slow)
                            ,@xl ,@yl
                            (:local.get ,x) (:local.get ,d0) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,y) (:local.get ,d1) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,off) (:local.get ,x) (:local.get ,d1) :i32.mul :i32.add (:local.get ,y) :i32.add
                            (:local.set ,k) (:local.get ,data) (:local.set ,d))
                          :vector d k
                          `((:local.get ,e) ,@xb ,@yb)))))))))
    (multiple-value-bind (kind shape)
        ;; a two-dimensional access to an array of unknown type: inline for
        ;; an array of elements of type T (a header over a simple vector)
        (if (ty-array-kind aty) (ty-array-kind aty) (values :vector :md2))
      ;; a simple vector of a known kind with a subscript of unknown type
      ;; (an element of a vector, a counter that may grow): the subscript is
      ;; tested to be a fixnum inline, as for an array of unknown type
      (when (and kind (not two) (not row-major) (eq shape :vec) (null (cdr subs))
                 (not (i32-arg-p (first subs)))
                 (or (not (member name '(char schar))) (eq kind :string)))
        (return-from array-locate-code (speculative-locate-code-1 name kind a (first subs) slow)))
      (when (and kind
                 ;; (two subscripts of unknown type: tested to be fixnums inline)
                 (or (every #'i32-arg-p subs) (and two (member shape '(:md2 :any))))
                 (or (not (member name '(char schar))) (eq kind :string))
                 (cond (two (member shape '(:md2 :any)))
                       (row-major t)
                       (t (member shape '(:vec :any)))))
        (let ((e (new-local)) (d (new-local `(:ref :null ,kind))) (k (new-local :i32)))
          (cond
            ((and (not two) (eq shape :vec) (eq (lvar-rep-of a) (ref-rep kind)))
             ;; a variable held as a reference to the storage: no test
             (values `(,@(cg-rep a (ref-rep kind)) (:local.set ,d) ,@(cg-rep (first subs) :i32) (:local.set ,k))
                     ;; (at (SAFETY 0) the subscript is trusted: the Wasm array's own test traps)
                     (unless (eql (cg-safety) 0)
                       `((:local.get ,k) (:local.get ,d) :array.len :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)))
                     kind d k
                     `((:local.get ,d) ,@(subscript-box-code (first subs) k))))
            ((and (eq shape :md2) (eq (lvar-rep-of a) (md2-rep kind)))
             ;; a variable held with its storage, dimensions and offset in locals
             (destructuring-bind (data d0 d1 off) (md2-shadows (second a))
               (let ((x (new-local :i32)) (y (new-local :i32)))
                (multiple-value-bind (xs xl xb) (subscript-i32-parts (first subs) x slow)
                 (multiple-value-bind (ys yl yb) (if two (subscript-i32-parts (second subs) y slow) (values nil nil nil))
                 (values
                  `(,@xs ,@ys)
                  `(,@xl ,@yl
                    ,@(if two
                          `((:local.get ,x) (:local.get ,d0) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,y) (:local.get ,d1) :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow)
                            (:local.get ,off) (:local.get ,x) (:local.get ,d1) :i32.mul :i32.add (:local.get ,y) :i32.add)
                          `((:local.get ,x) (:local.get ,d0) (:local.get ,d1) :i32.mul :i32.lt_u :i32.eqz
                            (:hint 0) (:br_if ,slow)
                            (:local.get ,off) (:local.get ,x) :i32.add))
                    (:local.set ,k) (:local.get ,data) (:local.set ,d))
                  kind d k
                  `(,@(cg a) ,@xb ,@yb)))))))
            ((and (not two) (member shape '(:vec :any)) (not (and row-major (eq shape :any))))
              ;; the array is the storage
              (values `(,@(cg a) (:local.set ,e) ,@(cg-rep (first subs) :i32) (:local.set ,k))
                      `((:local.get ,e) (:ref.test ,kind) :i32.eqz (:hint 0) (:br_if ,slow)
                        (:local.get ,e) (:ref.cast! ,kind) (:local.set ,d)
                        ,@(unless (eql (cg-safety) 0)
                            `((:local.get ,k) (:local.get ,d) :array.len :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow))))
                      kind d k
                      `((:local.get ,e) ,@(subscript-box-code (first subs) k))))
            (t
              ;; a header (with ROW-MAJOR-AREF of an array of any rank: only two-dimensional ones inline)
              (let ((m (new-local '(:ref :null :mdarray))) (s (new-local))
                    (x (new-local :i32)) (y (new-local :i32)) (d1 (new-local :i32)))
               (multiple-value-bind (xs xl xb) (subscript-i32-parts (first subs) x slow)
                (multiple-value-bind (ys yl yb) (if two (subscript-i32-parts (second subs) y slow) (values nil nil nil))
                (values
                 `(,@(cg a) (:local.set ,e) ,@xs ,@ys)
                 `(,@xl ,@yl
                   (:local.get ,e) (:ref.test :mdarray) :i32.eqz (:br_if ,slow)
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
                 `((:local.get ,e) ,@xb ,@yb))))))))))))

(defun speculative-locate-code (name a sub slow)
  "ARRAY-LOCATE-CODE for a one-dimensional access whose array is of unknown
type: the access is inline for the usual kind (a simple vector for AREF and
SVREF, a simple string for CHAR and SCHAR) and, for AREF and CHAR, for a
one-dimensional array header (a fill pointer, adjustable) over storage of
that kind; everything else goes to the helper, which handles every array."
  (let ((kind (case name
                ((clysm-sys::%aref1 svref clysm-sys::%aset1 clysm-sys::%svset) :vector)
                ((char schar clysm-sys::%charset clysm-sys::%scharset) :string))))
    (when kind
      (speculative-locate-code-1 name kind a sub slow))))

(defun speculative-locate-code-1 (name kind a sub slow)
  (let* ((header (member name '(clysm-sys::%aref1 clysm-sys::%aset1 char clysm-sys::%charset)))
         (e (new-local)) (d (new-local `(:ref :null ,kind))) (k (new-local :i32)) (k0 (new-local :i32))
         (fx (fx-arg-p sub)) (ie (unless fx (new-local)))
         (m (new-local '(:ref :null :mdarray))) (s (new-local)) (hit (gensym "HIT")))
    (progn
      (values `(,@(cg a) (:local.set ,e)
                ,@(if fx
                      `(,@(cg-rep sub :i32) (:local.set ,k0))
                      `(,@(cg sub) (:local.set ,ie))))
              `(,@(if fx
                      `((:local.get ,k0) (:local.set ,k))
                      `((:local.get ,ie) (:ref.test :i31) :i32.eqz (:hint 0) (:br_if ,slow)
                        (:local.get ,ie) (:ref.cast! :i31) :i31.get_s (:local.set ,k)))
                (:block ,hit nil
                  (:local.get ,e) (:ref.test ,kind)
                  (:if nil nil ((:local.get ,e) (:ref.cast! ,kind) (:local.set ,d) (:br ,hit)))
                  ,@(if header
                        `((:local.get ,e) (:ref.test :mdarray) :i32.eqz (:hint 0) (:br_if ,slow)
                          (:local.get ,e) (:ref.cast! :mdarray) (:local.tee ,m)
                          ;; a one-dimensional header: the subscript below its dimension
                          (:struct.get :mdarray 7) (:local.get ,k) :i32.le_u (:hint 0) (:br_if ,slow)
                          (:local.get ,m) (:struct.get :mdarray 0) (:local.tee ,s) (:ref.test ,kind) :i32.eqz
                          (:hint 0) (:br_if ,slow)
                          (:local.get ,s) (:ref.cast! ,kind) (:local.set ,d)
                          (:local.get ,m) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s
                          (:local.get ,k) :i32.add (:local.set ,k))
                        `((:br ,slow))))
                (:local.get ,k) (:local.get ,d) :array.len :i32.lt_u :i32.eqz (:hint 0) (:br_if ,slow))
              kind d k
              (if fx
                  `((:local.get ,e) (:local.get ,k0) :ref.i31)
                  `((:local.get ,e) (:local.get ,ie)))))))

(defun array-slow-returns-p (name args)
  "The generic function may return when the inline access does not apply:
ROW-MAJOR-AREF of an array of unknown rank (inline for two dimensions only),
and any access to an array of unknown type (inline for the usual kind only)."
  (or (null (ty-array-kind (node-ty (first args))))
      (and (member name '(row-major-aref clysm-sys::%row-major-aset))
           (eq (nth-value 1 (ty-array-kind (node-ty (first args)))) :any))))

(defun array-slow-call (name)
  (ecase name
    (clysm-sys::%aref1 '((:call :aref1)))
    (svref (lisp-call-instrs 'svref 2))       ; (signals unless a simple vector)
    (char '((:call :char-ref)))
    (schar '((:call :schar-ref)))
    (clysm-sys::%aset1 '((:call :aset1)))
    (clysm-sys::%svset '((:call :svset)))
    (clysm-sys::%charset '((:call :char-set)))
    (clysm-sys::%scharset '((:call :schar-set)))
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

(defun code-char-fixnum-arg (node)
  "The argument of NODE when it is (CODE-CHAR N) of a fixnum N, else NIL."
  (and (member (car node) '(:prim :call)) (eq (node-op-name node) 'code-char)
       (= (length (cddr node)) 1) (fx-arg-p (third node))
       (third node)))

(defun char-code-elt-code (node)
  "The code of the character read by NODE when it is a CHAR or SCHAR of a
string read inline (TYPED-ELT-CODE), as an i32: the element of the storage
as it is, with no character made and taken apart again (babel's encoders,
(CHAR-CODE (SCHAR S I)) per character).  NIL when NODE is not such a read."
  (when (and (member (car node) '(:prim :call)) (member (node-op-name node) '(char schar)))
    (let ((slow (gensym "SLOW")) (done (gensym "ELT")))
      (multiple-value-bind (setup locate kind d k slow-args) (array-locate-code (node-op-name node) (cddr node) slow)
        (when (and setup (eq kind :string))
          `(,@setup
            (:block ,done :i32
              (:block ,slow nil
                ,@locate
                (:local.get ,d) (:local.get ,k) ,@(storage-get-code kind)
                (:br ,done))
              ,@slow-args ,@(array-slow-call (node-op-name node))
              ,@(if (array-slow-returns-p (node-op-name node) (cddr node))
                    `((:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.sub)
                    '(:unreachable)))))))))

(defun trust-int-store-p (vty)
  "A value of type VTY stored into an array of integers is trusted to be an
element: at (SAFETY 0), an integer within 64 bits."
  (and (eql (cg-safety) 0) (ty-int64-p vty)))

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
                 ;; (SETF (SCHAR S I) (CODE-CHAR N)) of a fixnum N: the code
                 ;; stored as it is (babel's decoders), tested to be one
                 ;; unless its type says so
                 (code (and (eq kind :string) (code-char-fixnum-arg val)))
                 (proven (case kind
                           (:vector :eqref)
                           (:string (and code :i32))
                           (:f32vec (and (ty-single-p vty) :f32))
                           (:f64vec (and (ty-double-p vty) :f64))
                           ((:u64vec :u63vec) (and (ty-in-range-p vty 0 (1- (expt 2 63))) :i64))
                           ((:s64vec) (and (ty-int64-p vty) :i64))
                           ((:u32vec) (and (or (ty-in-range-p vty lo hi) (trust-int-store-p vty)) :i64))
                           (t (cond ((ty-in-range-p vty lo hi) :i32)
                                    ;; (SAFETY 0): an integer within 64 bits is
                                    ;; trusted to be an element (wrapped)
                                    ((and (integerp lo) (trust-int-store-p vty)) :wrap)))))
                 (x (unless proven (new-local))))
            (when (or proven (member kind '(:string :bits :u7vec :bytes :s8vec :u15vec :u16vec :s16vec :f32vec :f64vec)))
              `(,@setup
                ,@(if proven
                      `(,@(cg-rep (if code code val) (if (eq proven :wrap) :i64 proven))
                        ,@(when (or (eq kind :u32vec) (eq proven :wrap)) '(:i32.wrap_i64)) (:local.set ,raw))
                      `(,@(cg val) (:local.set ,x)))
                (:block ,done nil
                  (:block ,slow nil
                    ,@locate
                    ,@(unless proven (storage-unbox-code kind x raw `((:br ,slow))))
                    ,@(when (and code (not (ty-in-range-p (node-ty code) 0 (1- char-code-limit*))))
                        `((:local.get ,raw) (:i32.const ,char-code-limit*) :i32.ge_u (:hint 0) (:br_if ,slow)))
                    (:local.get ,d) (:local.get ,k) (:local.get ,raw) (:array.set ,kind)
                    (:br ,done))
                  ,@slow-args
                  ,@(if proven `((:local.get ,raw) ,@(storage-box-code kind)) `((:local.get ,x)))
                  ,@(array-slow-call name)
                  ,@(if (array-slow-returns-p name (cddr node)) '(:drop) '(:unreachable)))
                ,@(unless effect
                    (if proven `((:local.get ,raw) ,@(storage-box-code kind)) `((:local.get ,x))))))))))))

(defun replace-copy-code (node)
  "(%REPLACE A B 0 NIL 0 NIL), REPLACE of two whole simple vectors over the
same kind of storage: one array.copy of the shorter length (it copies
overlapping ranges as if through a buffer), else NIL."
  (destructuring-bind (a b s1 e1 s2 e2) (cddr node)
    (flet ((vec-kind (x) (multiple-value-bind (kind shape) (ty-array-kind (node-ty x))
                           (and (eq shape :vec) kind)))
           (const-p (x v) (and (eq (car x) :const) (eql (second x) v))))
      (let ((ka (vec-kind a)) (kb (vec-kind b)))
        (when (and ka (eq ka kb) (const-p s1 0) (const-p s2 0) (const-p e1 nil) (const-p e2 nil))
          (let ((da (new-local `(:ref :null ,ka))) (db (new-local `(:ref :null ,ka))) (r (new-local)))
            `(,@(cg a) (:local.set ,r) ,@(cg b) (:ref.cast ,ka) (:local.set ,db)
              (:local.get ,r) (:ref.cast ,ka) (:local.set ,da)
              (:local.get ,da) (:i32.const 0) (:local.get ,db) (:i32.const 0)
              (:local.get ,da) :array.len (:local.get ,db) :array.len
              (:local.get ,da) :array.len (:local.get ,db) :array.len :i32.lt_u :select
              (:array.copy ,ka ,ka)
              (:local.get ,r))))))))

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
      ;; setting the car or cdr of what is known to be a cons: no check
      ((clysm-sys::%setcar clysm-sys::%setcdr rplaca rplacd)
       (when (and (ty-only-p aty +k-cons+) (= (length args) 2))
         (let* ((field (if (member name '(clysm-sys::%setcar rplaca)) 0 1))
                ;; RPLACA/RPLACD return the cons: it is kept in a local of
                ;; the cons type (an eqref local would lose the type the
                ;; store needs)
                (tmp (new-local (if (member name '(rplaca rplacd)) '(:ref :null :cons) :eqref))))
           (if (member name '(rplaca rplacd))
               `(,@(cg a) (:ref.cast! :cons) (:local.tee ,tmp) ,@(cg (second args))
                 (:struct.set :cons ,field) (:local.get ,tmp))
               `(,@(cg a) (:ref.cast! :cons) ,@(cg (second args)) (:local.tee ,tmp)
                 (:struct.set :cons ,field) (:local.get ,tmp))))))
      (clysm-sys::%svref
       (when (ty-only-p aty +k-svec+)
         `(,@(cg a) (:ref.cast! :vector) ,@(index-i32-code (second args)) (:array.get :vector))))
      (clysm-sys::%set-svref
       (when (ty-only-p aty +k-svec+)
         (let ((tmp (new-local)))
           `(,@(cg a) (:ref.cast! :vector) ,@(index-i32-code (second args))
             ,@(cg (third args)) (:local.tee ,tmp) (:array.set :vector) (:local.get ,tmp))))))))

(defun struct-iref-args (node)
  "The object, class, index, value (or NIL) and size nodes of a %STRUCT-IREF
or %STRUCT-ISET node, and whether it sets."
  (let ((args (cddr node)))
    (if (eq (node-op-name node) 'clysm-sys::%struct-iset)
        (values (first args) (second args) (third args) (fourth args) (fifth args) t)
        (values (first args) (second args) (third args) nil (fourth args) nil))))

;;; ------------------------------------------------------------
;;; Structures with typed slots (runtime/struct.lisp)
;;; ------------------------------------------------------------
;;; A structure some of whose slots are declared DOUBLE-FLOAT has a Wasm
;;; type of its own: its slots of kinds KINDS (:D an f64 field, :E an eqref
;;; one) after the $instance fields.  The type of KINDS is a subtype of the
;;; type of KINDS but the last (of $siK when those are all :E), so a
;;; structure including another is of a subtype of its type.  Each module
;;; that uses the type defines it (types out of the base recursion group are
;;; equal across modules when they are defined alike).

(defun tshape-type (kinds)
  (cond ((null kinds) :sfield)
        ((not (member :d kinds)) (sinst-type (length kinds)))
        (t (let ((name (intern (format nil "TS-~{~A~}" kinds) :keyword)))
             (unless (gethash name (clysm-wasm::wasm-module-type-table *m*))
               (let ((super (tshape-type (butlast kinds))))
                 (add-type *m* name
                           `(:sub (,super) nil
                                  (:struct ((:eqref t) ((:ref :vector) t) (:i32 t)
                                            ,@(mapcar (lambda (k) (if (eq k :d) '(:f64 t) '(:eqref t))) kinds)))))))
             name))))

(defun tstruct-kinds (node)
  (and (eq (car node) :const) (consp (second node)) (every (lambda (k) (member k '(:d :e))) (second node))
       (<= (length (second node)) +struct-inline-max+)
       (second node)))

(defun tstruct-field-check-code (o c ty s l slow)
  "Code branching to SLOW unless local O is of the type TY (left in S) and of
the structure class (local C) or one including it."
  (let ((hit (gensym "HIT")))
    `((:block ,hit nil
        (:local.get ,o) (:ref.test ,ty) :i32.eqz (:hint 0) (:br_if ,slow)
        (:local.get ,o) (:ref.cast! ,ty) (:local.tee ,s) (:struct.get ,ty 0) (:local.tee ,l) (:local.get ,c)
        :ref.eq (:hint 1) (:br_if ,hit)
        ,@(inherits-check-code l c '((:i32.const 0))) :i32.eqz (:br_if ,slow)))))

(defun struct-class-known-p (x cls)
  "The object (node X) is known to be of the structure class whose node is
CLS (a declaration of the variable, a test, a slot access before)."
  (let ((name (struct-class-node-name cls)))
    (and name (eq (ty-cls (node-ty x)) name))))

(defun struct-class-shape-type (name)
  "The Wasm type of the instances of the structure class NAME when its slots
are fields or typed (its own instances are of exactly that type), else NIL."
  (let ((desc (get name 'clysm-sys::%struct-description)))
    (when (and (consp desc) (null (car desc)) (listp (second desc)))
      (let ((rep (nth 6 desc)))
        (case rep
          (:fields (let ((size (third desc))) (and (integerp size) (<= 1 size +struct-inline-max+) (sinst-type size))))
          (:typed (let ((kinds (mapcar (lambda (sl) (if (member (third sl) '(double-float long-float)) :d :e))
                                       (second desc))))
                    (and (<= 1 (length kinds) +struct-inline-max+) (member :d kinds)
                         (tshape-type kinds)))))))))

;;; A variable bound once (never assigned) to an object known to be of a
;;; structure class whose slots are fields has a shadow local of the Wasm
;;; type of the class: the object cast where the variable is bound, or null
;;; (an instance of an including structure that has a slot vector).  Its
;;; accessors test the shadow for null and use it, one test of the object's
;;; type per binding instead of one per access (which the engine does not
;;; share between the accesses: n-body took 1.35 times as long).
(defun struct-shadow-type (lv)
  (let ((ty (lvar-type lv)))
    (and ty (not (lvar-mutated lv)) (not (lvar-special lv)) (not (boxed-p lv))
         (ty-cls ty) (symbolp (ty-cls ty)) (struct-class-type-p (ty-cls ty))
         (struct-class-shape-type (ty-cls ty)))))

(defun struct-shadow-init-code (lv idx)
  "Code setting the shadow of LV (in local IDX), when it has one."
  (let* ((ty (struct-shadow-type lv))
         (from (and ty *binding-init* (struct-shadow-of *binding-init* ty))))
    ;; (for a variable not declared, only a copy: its accesses are casts
    ;; otherwise, cheaper than a test where it is bound)
    (when (and ty (or from (lvar-decl lv)))
      (let ((sh (new-local `(:ref :null ,ty))))
        (setf (gethash lv (fstate-shadows *fs*)) (cons ty sh))
        (if from
            ;; (bound to a variable that has one: the variable of a SETF expansion)
            `((:local.get ,from) (:local.set ,sh))
        `((:local.get ,idx) (:ref.test ,ty)
          (:if nil (:ref :null ,ty) ((:local.get ,idx) (:ref.cast! ,ty)) ((:ref.null ,ty)))
          (:local.set ,sh)))))))

(defun struct-shadow-of (x ty)
  "The shadow local of the variable X (a node) when it is of the Wasm type TY."
  (when (eq (car x) :lref)
    (let ((e (gethash (second x) (fstate-shadows *fs*))))
      (and (consp e) (eq (car e) ty)
           (eq (car (get-loc (second x))) :local)
           (cdr e)))))

(defun known-struct-check-code (o ty s slow &optional x)
  "For an object (local O) known to be of the structure class: code leaving
it in S as the Wasm type TY, branching to SLOW where it may not be of it."
  (let ((sh (and x (struct-shadow-of x ty))))
    (cond ((and (loop for k from 1 to +struct-inline-max+ thereis (eq ty (sinst-type k)))
                (or (eql (cg-safety) 0)
                    ;; (not by a declaration: by an access before, a
                    ;; constructor, a type test)
                    (and x (eq (car x) :lref) (not (lvar-decl (second x))))))
           ;; a structure of field slots: every instance of its class, or
           ;; of one including it, is of its Wasm type (one of more slots
           ;; is a big structure, runtime/struct.lisp): a cast
           `((:local.get ,o) (:ref.cast! ,ty) (:local.set ,s)))
          (sh
           ;; (a declared variable: one test where it is bound; a value not
           ;; of the type declared takes the function, which signals)
           `((:local.get ,sh) :ref.is_null (:hint 0) (:br_if ,slow) (:local.get ,sh) (:local.set ,s)))
          (t
           ;; another declared object, or a structure of typed slots, whose
           ;; instances may have a slot vector (made by the interpreter,
           ;; restored from a heap image, of an including structure of more
           ;; than 32 slots): a test (a BR_ON_CAST measured slower than the
           ;; test and the cast)
           `((:local.get ,o) (:ref.test ,ty) :i32.eqz (:hint 0) (:br_if ,slow)
             (:local.get ,o) (:ref.cast! ,ty) (:local.set ,s))))))

(defun tstruct-ref-code (node rep)
  "%STRUCT-TREF in REP (:EQREF, or :F64 for a slot of kind :D), or NIL."
  (destructuring-bind (x cls i kinds-node) (cddr node)
    (let ((k (const-int-node i)) (kinds (tstruct-kinds kinds-node)))
      (when (and k kinds (< -1 k (length kinds))
                 (or (eq rep :eqref) (and (eq rep :f64) (eq (nth k kinds) :d))))
        (let* ((ty (tshape-type kinds)) (d (eq (nth k kinds) :d)) (known (struct-class-known-p x cls))
               (o (new-local)) (c (new-local)) (s (new-local `(:ref :null ,ty))) (l (new-local))
               (slow (gensym "SLOW")) (done (gensym "TREF")))
          ;; (the class is only loaded where it is compared)
          `(,@(cg x) (:local.set ,o) ,@(unless known `(,@(cg cls) (:local.set ,c)))
            ,@(cg-effect i) ,@(cg-effect kinds-node)
            (:block ,done ,(if (eq rep :f64) :f64 :eqref)
              (:block ,slow nil
                ,@(if known
                      (known-struct-check-code o ty s slow x)
                      (tstruct-field-check-code o c ty s l slow))
                (:local.get ,s) (:struct.get ,ty ,(sinst-field k))
                ,@(when (and d (eq rep :eqref)) '((:struct.new :double)))
                (:br ,done))
              (:local.get ,o) ,@(if known (cg cls) `((:local.get ,c))) (:i32.const ,k) :ref.i31
              ,@(lisp-call-instrs 'clysm-sys::%struct-ref 3)
              ,@(when (eq rep :f64) (unbox-rep-code :f64 (mkty +k-double+))))))))))

(defun tstruct-set-code (node effect)
  "%STRUCT-TSET; with EFFECT leaving nothing, else the value."
  (destructuring-bind (x cls i v kinds-node) (cddr node)
    (let ((k (const-int-node i)) (kinds (tstruct-kinds kinds-node)))
      (when (and k kinds (< -1 k (length kinds)))
        (let* ((ty (tshape-type kinds)) (d (eq (nth k kinds) :d)) (raw (and d (ty-double-p (node-ty v))))
               (known (struct-class-known-p x cls))
               (o (new-local)) (c (new-local)) (s (new-local `(:ref :null ,ty))) (l (new-local))
               (val (new-local)) (f (new-local :f64)) (slow (gensym "SLOW")) (done (gensym "TSET")))
          `(,@(cg x) (:local.set ,o) ,@(unless known `(,@(cg cls) (:local.set ,c))) ,@(cg-effect i)
            ,@(if raw `(,@(cg-rep v :f64) (:local.set ,f)) `(,@(cg v) (:local.set ,val)))
            ,@(cg-effect kinds-node)
            (:block ,done nil
              (:block ,slow nil
                ,@(if known
                      (known-struct-check-code o ty s slow x)
                      (tstruct-field-check-code o c ty s l slow))
                ,@(cond ((not d) `((:local.get ,s) (:local.get ,val) (:struct.set ,ty ,(sinst-field k))))
                        (raw `((:local.get ,s) (:local.get ,f) (:struct.set ,ty ,(sinst-field k))))
                        (t `((:local.get ,val) (:ref.test :double)
                             (:if nil nil
                                  ((:local.get ,s) (:local.get ,val) (:ref.cast! :double) (:struct.get :double 0)
                                   (:struct.set ,ty ,(sinst-field k)))
                                  ((:local.get ,val) (:global.get ,(sym-global 'double-float))
                                   ,@(lisp-call-instrs 'clysm-sys::%type-error 2) :drop)))))
                (:br ,done))
              (:local.get ,o) ,@(if known (cg cls) `((:local.get ,c))) (:i32.const ,k) :ref.i31
              ,@(if raw `((:local.get ,f) (:struct.new :double)) `((:local.get ,val)))
              ,@(lisp-call-instrs 'clysm-sys::%struct-set 4) :drop)
            ,@(unless effect
                (if raw `((:local.get ,f) (:struct.new :double)) `((:local.get ,val))))))))))

(defun tstruct-make-code (node)
  "%MAKE-TSTRUCT (all the slots), %MAKE-TSTRUCT-DEFAULT (none set) or
%COPY-TSTRUCT of a structure with typed slots, or NIL."
  (let ((name (node-op-name node)) (args (cddr node)))
    (case name
      (clysm-sys::%make-tstruct
       (let ((kinds (tstruct-kinds (second args))) (vals (cddr args)))
         (when (and kinds (= (length vals) (length kinds)))
           (let* ((ty (tshape-type kinds)) (c (new-local))
                  (locals (mapcar (lambda (k v) (if (and (eq k :d) (ty-double-p (node-ty v))) (new-local :f64) (new-local)))
                                  kinds vals)))
             `(,@(cg (first args)) (:local.set ,c) ,@(cg-effect (second args))
               ,@(loop for k in kinds for v in vals for loc in locals
                       append (if (and (eq k :d) (ty-double-p (node-ty v)))
                                  `(,@(cg-rep v :f64) (:local.set ,loc))
                                  `(,@(cg v) (:local.set ,loc))))
               (:local.get ,c) (:local.get ,c) (:call :layout-inherits) (:i32.const 0)
               ,@(loop for k in kinds for v in vals for loc in locals
                       append (cond ((not (eq k :d)) `((:local.get ,loc)))
                                    ((ty-double-p (node-ty v)) `((:local.get ,loc)))
                                    (t `((:local.get ,loc) (:ref.test :double)
                                         (:if nil :f64 ((:local.get ,loc) (:ref.cast! :double) (:struct.get :double 0))
                                              ((:local.get ,loc) (:global.get ,(sym-global 'double-float))
                                               ,@(lisp-call-instrs 'clysm-sys::%type-error 2) :drop
                                               (:f64.const 0d0)))))))
               (:struct.new ,ty))))))
      (clysm-sys::%make-tstruct-default
       (let ((kinds (tstruct-kinds (second args))) (c (new-local)))
         (when kinds
           `(,@(cg (first args)) (:local.set ,c) ,@(cg-effect (second args))
             (:local.get ,c) (:local.get ,c) (:call :layout-inherits) (:i32.const 0)
             ,@(mapcar (lambda (k) (if (eq k :d) '(:f64.const 0d0) '(:ref.null :eq))) kinds)
             (:struct.new ,(tshape-type kinds))))))
      (clysm-sys::%copy-tstruct
       (let ((kinds (tstruct-kinds (third args))))
         (when kinds
           (let ((ty (tshape-type kinds)) (s (new-local)))
             `(,@(cg (first args)) (:local.set ,s) ,@(cg-effect (second args)) ,@(cg-effect (third args))
               (:local.get ,s) (:ref.cast ,ty) (:struct.get ,ty 0) (:local.get ,s) (:ref.cast! ,ty) (:struct.get ,ty 1)
               (:i32.const 0)
               ,@(loop for i below (length kinds)
                       append `((:local.get ,s) (:ref.cast! ,ty) (:struct.get ,ty ,(sinst-field i))))
               (:struct.new ,ty)))))))))

(defun typed-struct-code (node)
  "Slot access of an object known to be of the structure class: no test of its class."
  (when (member (node-op-name node) '(clysm-sys::%struct-iref clysm-sys::%struct-iset))
    ;; slots in fields of their own: a cast to the structure's type
    (return-from typed-struct-code
      (multiple-value-bind (x cls i v size vp) (struct-iref-args node)
        (let* ((name (struct-class-node-name cls)) (k (const-int-node i))
               (n (let ((s (const-int-node size))) (and s (mod s 64))))
               (ty (and k n (< -1 k n) (<= n +struct-inline-max+) (sinst-type n))))
          (when (and ty name (eq (ty-cls (node-ty x)) name))
            ;; (an instance of a structure including it that has a slot vector: the function)
            (let ((o (new-local)) (s (new-local `(:ref :null ,ty))) (tmp (new-local))
                  (slow (gensym "SLOW")) (done (gensym "SREF")))
              `(,@(cg x) (:local.set ,o) ,@(when vp `(,@(cg v) (:local.set ,tmp)))
                (:block ,done :eqref
                  (:block ,slow nil
                    ,@(known-struct-check-code o ty s slow x)
                    (:local.get ,s)
                    ,@(if vp
                          `((:local.get ,tmp) (:struct.set ,ty ,(sinst-field k)) (:local.get ,tmp))
                          `((:struct.get ,ty ,(sinst-field k))))
                    (:br ,done))
                  (:local.get ,o) ,@(cg cls) (:i32.const ,k) :ref.i31
                  ,@(if vp
                        `((:local.get ,tmp) ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))
                        (lisp-call-instrs 'clysm-sys::%struct-ref 3))))))))))
  (destructuring-bind (x cls i &optional (v nil vp))
      ;; (without the shape, the last argument when there is one)
      (let ((args (cddr node)))
        (if (= (length args) (if (eq (node-op-name node) 'clysm-sys::%struct-set) 5 4)) (butlast args) args))
    (let ((name (struct-class-node-name cls)))
      (when (and name (eq (ty-cls (node-ty x)) name) (fx-arg-p i))
        (if vp
            (let ((tmp (new-local)))
              `(,@(cg x) (:ref.cast! :instance) (:struct.get :instance 1) ,@(cg-rep i :i32)
                ,@(cg v) (:local.tee ,tmp) (:array.set :vector) (:local.get ,tmp)))
            `(,@(cg x) (:ref.cast! :instance) (:struct.get :instance 1) ,@(cg-rep i :i32)
              (:array.get :vector)))))))

(defun known-test-result (name args)
  "When the types of the arguments decide the test NAME of ARGS: :TRUE or
:FALSE (the check of a value given to a variable declared of an integer
type, where the type of the value shows it holds), else NIL."
  (let ((tys (mapcar #'node-ty args)))
    (when (notany #'ty-bottom-p tys)
      (case name
        ((integerp clysm-sys::%integerp) (when (= (length tys) 1)
                    (cond ((ty-only-p (first tys) +k-int+) :true)
                          ((not (ty-int-p (first tys))) :false))))
        (clysm-sys::%simple-string-p
         (when (= (length tys) 1)
           (let ((ty (first tys)))
             (cond ((not (logtest (ty-bits ty) +k-string+)) :false)
                   ((ty-only-p ty +k-string+) :true)))))
        ((clysm-sys::%simple-array-p clysm-sys::%simple-vector-p simple-vector-p)
         (when (= (length tys) 1)
           (let ((ty (first tys)))
             (if (member name '(clysm-sys::%simple-vector-p simple-vector-p))
                 (cond ((not (logtest (ty-bits ty) +k-svec+)) :false)
                       ((and (ty-only-p ty +k-svec+) (null (ty-cls ty))) :true))
                 (cond ((not (logtest (ty-bits ty) (logior +k-svec+ +k-string+ +k-other+))) :false)
                       ((ty-vec-kind ty) :true))))))
        ((clysm-sys::%bignump clysm-sys::bignump)
         (when (= (length tys) 1)
           (cond ((not (ty-int-p (first tys))) :false)
                 ((ty-fixnum-p (first tys)) :false))))
        ((< > <= >= = /=)
         (when (= (length tys) 2)
           (let ((a (first tys)) (b (second tys)))
             (when (and (ty-only-p a +k-int+) (ty-only-p b +k-int+))
               (let ((alo (ty-lo a)) (ahi (ty-hi a)) (blo (ty-lo b)) (bhi (ty-hi b)))
                 (flet ((lt (x y) (and x y (< x y))) (le (x y) (and x y (<= x y))))
                   (ecase name
                     (< (cond ((lt ahi blo) :true) ((le bhi alo) :false)))
                     (> (cond ((lt bhi alo) :true) ((le ahi blo) :false)))
                     (<= (cond ((le ahi blo) :true) ((lt bhi alo) :false)))
                     (>= (cond ((le bhi alo) :true) ((lt ahi blo) :false)))
                     (= (cond ((or (lt ahi blo) (lt bhi alo)) :false)
                              ((and alo (eql alo ahi) (eql blo bhi) (eql alo blo)) :true)))
                     (/= (cond ((or (lt ahi blo) (lt bhi alo)) :true)
                               ((and alo (eql alo ahi) (eql blo bhi) (eql alo blo)) :false))))))))))))))

(defun typed-test-code (node)
  "i32 code for a numeric comparison or predicate using the argument types, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node))
         (a (first args)) (b (second args)))
    (let ((known (known-test-result name args)))
      (when known
        (return-from typed-test-code
          `(,@(loop for x in args append (cg-effect x)) (:i32.const ,(if (eq known :true) 1 0))))))
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
                  ,(ecase name (< :f64.lt) (> :f64.gt) (<= :f64.le) (>= :f64.ge) (= :f64.eq) (/= :f64.ne))))
               ((dual-cmp-code node))
               ((spec-cmp-code node))
               ((wide-cmp-code node)))))
      ((char= char/=)
       ;; two characters (known: a declared character, a constant): the
       ;; same immediates, no check
       (when (and (= (length args) 2) (ty-only-p (node-ty a) +k-char+) (ty-only-p (node-ty b) +k-char+))
         `(,@(cg a) ,@(cg b) :ref.eq ,@(when (eq name 'char/=) '(:i32.eqz)))))
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
      (clysm-sys::fixnump
       ;; (the check of a value given to a variable declared FIXNUM)
       (cond ((fx-arg-p a) `(,@(cg-effect a) (:i32.const 1)))
             ((i32-arg-p a)
              `(,@(cg-rep a :i32) (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u))
             ((i64-arg-p a)
              `(,@(cg-rep a :i64) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u))))
      ((clysm-sys::%bignump clysm-sys::bignump)
       ;; an integer within 64 bits is a bignum when it is not a fixnum
       (when (i64-arg-p a)
         `(,@(cg-rep a :i64) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.ge_u)))
      ((eq eql)
       (when (= (length args) 2)
         (cond ((and (fx-arg-p a) (fx-arg-p b)
                     (or (eq (lvar-rep-of a) :i32) (eq (lvar-rep-of b) :i32)))
                `(,@(cg-rep a :i32) ,@(cg-rep b :i32) :i32.eq))
               ;; EQL of a value that is no boxed number (a fixnum, a
               ;; character, a symbol, a cons, ...) is EQ, without the
               ;; helper (a key compared in MAPHASH, (eql x 'sym))
               ((and (eq name 'eql) (or (ty-eq-comparable-p (node-ty a)) (ty-eq-comparable-p (node-ty b))))
                `(,@(cg a) ,@(cg b) :ref.eq)))))
      (t (numeric-test-code node)))))

(defun f64-evident-p (node)
  "NODE is a double, or a double on one of its paths (an unboxed double
variable, CG-DUAL-F64)."
  (or (ty-double-p (node-ty node)) (fdual-lref-p node) (dual-f64-node-p node)))

(defun spec-cmp-code (node)
  "For a comparison of two numbers one of which is a double (or may be one,
unboxed) and the other may be one: i32 code comparing f64s when both are
doubles, else the generic comparison; or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (when (and (spec-cmp-node-p node) (not *no-fuse*)
               (or (f64-evident-p a) (f64-evident-p b))
               (logtest (ty-bits (node-ty a)) +k-double+) (logtest (ty-bits (node-ty b)) +k-double+))
      (let ((slow (gensym "SLOW")) (done (gensym "CMP"))
            (ba (new-local)) (fa (new-local :f64)) (bb (new-local)) (fb (new-local :f64))
            (p (if (eq (car node) :prim) (second node) (find-primitive name 2))))
        (when p
          (flet ((operand (n bl fl)
                   (cond ((ty-double-p (node-ty n))
                          `(,@(cg-rep n :f64) (:local.set ,fl) (:ref.null :eq) (:local.set ,bl)))
                         ((or (fdual-lref-p n) (dual-f64-node-p n)) (cg-dual-f64 n bl fl))
                         (t `(,@(cg n) (:local.set ,bl)))))
                 (unbox (bl fl)
                   `((:local.get ,bl) :ref.is_null
                     (:if nil :f64 ((:local.get ,fl))
                          ((:local.get ,bl) (:ref.test :double) :i32.eqz (:br_if ,slow)
                           (:local.get ,bl) (:ref.cast! :double) (:struct.get :double 0))))))
            `(,@(operand a ba fa) ,@(operand b bb fb)
              (:block ,done :i32
                (:block ,slow nil
                  ,@(unbox ba fa) ,@(unbox bb fb)
                  ,(ecase name (< :f64.lt) (> :f64.gt) (<= :f64.le) (>= :f64.ge) (= :f64.eq) (/= :f64.ne))
                  (:br ,done))
                ,@(funcall (prim-fn p) (list (fdual-materialize-code ba fa) (fdual-materialize-code bb fb)))))))))))

;;; A comparison of an integer accumulator (an i64 beside its eqref local,
;;; below) with a number: in i64 when both are integers within 64 bits, so
;;; that the accumulator is not made into an integer (a loop's counter
;;; compared with its limit); the generic comparison otherwise.
(defun dual-lref-p (node)
  (and (eq (car node) :lref) (dual-local (second node)) (eq (car (get-loc (second node))) :local)))

(defun dual-cmp-code (node)
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (when (and (spec-cmp-node-p node) (not *no-fuse*) (or (dual-lref-p a) (dual-lref-p b))
               (logtest (ty-bits (node-ty a)) +k-int+) (logtest (ty-bits (node-ty b)) +k-int+))
      (let ((slow (gensym "SLOW")) (done (gensym "CMP"))
            (ba (new-local)) (va (new-local :i64)) (bb (new-local)) (vb (new-local :i64))
            (p (if (eq (car node) :prim) (second node) (find-primitive name 2))))
        (when p
          (flet ((operand (n bl vl)
                   (if (dual-lref-p n)
                       `((:local.get ,(cdr (get-loc (second n)))) (:local.set ,bl)
                         (:local.get ,(dual-local (second n))) (:local.set ,vl))
                       `(,@(cg n) (:local.set ,bl)))))
            `(,@(operand a ba va) ,@(operand b bb vb)
              (:block ,done :i32
                (:block ,slow nil
                  ,@(dual-pair-i64-code ba va slow) ,@(dual-pair-i64-code bb vb slow)
                  ,(ecase name (< :i64.lt_s) (> :i64.gt_s) (<= :i64.le_s) (>= :i64.ge_s) (= :i64.eq) (/= :i64.ne))
                  (:br ,done))
                ,@(funcall (prim-fn p) (list (dual-materialize-code ba va) (dual-materialize-code bb vb)))))))))))

;;; A comparison of an integer known to be within 64 bits but not to be a
;;; fixnum (an element of a vector of (signed-byte 32) or (unsigned-byte
;;; 32), an i64 value) with a number of unknown type: in i64 when the other
;;; is an integer within 64 bits (a fixnum inline, a bignum by one call that
;;; reads its value), so that the known side is not boxed (a bignum
;;; allocated to compare a Unix time with a table of transitions); the
;;; generic comparison otherwise.
(defun wide-cmp-code (node)
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (when (and (spec-cmp-node-p node) (not *no-fuse*)
               (logtest (ty-bits (node-ty a)) +k-int+) (logtest (ty-bits (node-ty b)) +k-int+)
               (or (and (i64-arg-p a) (not (ty-fixnum-p (node-ty a))) (not (i64-arg-p b)))
                   (and (i64-arg-p b) (not (ty-fixnum-p (node-ty b))) (not (i64-arg-p a)))))
      (let ((slow (gensym "SLOW")) (done (gensym "CMP"))
            (ba (new-local)) (va (new-local :i64)) (bb (new-local)) (vb (new-local :i64))
            (p (if (eq (car node) :prim) (second node) (find-primitive name 2))))
        (when p
          (flet ((operand (n bl vl)
                   (if (i64-arg-p n)
                       `(,@(cg-rep n :i64) (:local.set ,vl))
                       `(,@(cg n) (:local.set ,bl))))
                 (decode (n bl vl)
                   (unless (i64-arg-p n)
                     `(,@(i64-decode-code `((:local.get ,bl)) slow) (:local.set ,vl))))
                 (box (n bl vl)
                   (if (i64-arg-p n)
                       `((:local.get ,vl) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+))
                         :i64.lt_u
                         (:if nil :eqref ((:local.get ,vl) :i32.wrap_i64 :ref.i31) ((:local.get ,vl) (:call :i64-int))))
                       `((:local.get ,bl)))))
            `(,@(operand a ba va) ,@(operand b bb vb)
              (:block ,done :i32
                (:block ,slow nil
                  ,@(decode a ba va) ,@(decode b bb vb)
                  (:local.get ,va) (:local.get ,vb)
                  ,(ecase name (< :i64.lt_s) (> :i64.gt_s) (<= :i64.le_s) (>= :i64.ge_s) (= :i64.eq) (/= :i64.ne))
                  (:br ,done))
                ,@(funcall (prim-fn p) (list (box a ba va) (box b bb vb)))))))))))

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

(defconstant +f64-int-limit+ 2251799813685248d0
  "2^51: an integral double of smaller magnitude converts by INTEGRAL-F64-I64-CODE.")

(defun integral-f64-i64-code (q)
  "The i64 of the integral f64 in the local Q, of magnitude below 2^51: Q
plus 1.5 * 2^52 has the integer in its low bits.  (No conversion
instruction: the engine makes those of a scratch register, whose upper part
each one also reads, which chains the iterations of a loop through it.)"
  `((:local.get ,q) (:f64.const 6755399441055744d0) :f64.add :i64.reinterpret_f64
    (:i64.const #x4338000000000000) :i64.sub))

(defun float-quotient-node-p (node)
  "NODE is the integer quotient of FLOOR TRUNCATE CEILING or ROUND of doubles, computed inline."
  (and (not *no-fuse*) (member (car node) '(:call :prim))
       (member (node-op-name node) '(floor truncate ceiling round))
       (float-divmod-node-p node)))

(defun typed-float-divmod (node want &optional need-rem pair)
  "Division of doubles (or a double and a 32-bit integer) inline, by the
algorithm of the helper :divmod, which is called when a value is not finite
or the divisor is zero.  WANT: :quotient (the integer quotient is left on the
stack), :fquotient (the quotient as an f64), :remainder (nothing is left) or
:pair (nothing is left: the quotient is stored in PAIR, the eqref and i64
locals of a leaf of fused integer arithmetic, the eqref null when the i64
holds it).
Returns the code and the f64 local that holds the remainder (it is not
computed for a division by 1 unless NEED-REM)."
  (let* ((args (cddr node)) (mode (cdr (assoc (node-op-name node) *divmod-modes*)))
         (da (new-local :f64)) (db (new-local :f64)) (q (new-local :f64)) (r (new-local :f64))
         (tmp (new-local :f64)) (adj (new-local :i32)) (w (new-local :i64))
         (slow (gensym "SLOW")) (done (gensym "DIV"))
         (unit (null (second args))))
    (if (and unit (not need-rem) (not (eq want :remainder)))
        ;; one argument, the quotient only: the rounding instruction
        (let ((op (ecase mode (0 :f64.trunc) (1 :f64.floor) (2 :f64.ceil) (3 :f64.nearest))))
          (ecase want
            (:pair
             `(,@(f64-arg-code (first args)) ,op (:local.tee ,q) :f64.abs (:f64.const ,+f64-int-limit+) :f64.lt
               (:hint 1) (:if nil nil
                    (,@(integral-f64-i64-code q) (:local.set ,(cdr pair)) (:ref.null :eq) (:local.set ,(car pair)))
                    ((:local.get ,q) (:call :f64-int) (:local.set ,(car pair))))))
            (:quotient
             `(,@(f64-arg-code (first args)) ,op (:local.tee ,q) :f64.abs (:f64.const 536870912d0) :f64.lt
               (:hint 1) (:if nil :eqref
                    (,@(integral-f64-i64-code q) :i32.wrap_i64 :ref.i31)
                    ;; (signals when it is not finite)
                    ((:local.get ,q) (:call :f64-int)))))
            (:fquotient
             `(,@(f64-arg-code (first args)) (:local.tee ,da) ,op (:local.tee ,q)
               (:local.get ,q) :f64.sub (:f64.const 0) :f64.ne
               (:hint 0) (:if nil :f64
                    ((:local.get ,da) (:struct.new :double) ,@(fixnum-code 1) (:i32.const ,mode) (:call :divmod)
                     (:call :to-f64))
                    ((:local.get ,q)))))))
        (values
         `(,@(f64-arg-code (first args)) (:local.set ,da)
           ,@(if unit '((:f64.const 1d0)) (f64-arg-code (second args))) (:local.set ,db)
           (:block ,done ,(ecase want (:quotient :eqref) (:fquotient :f64) ((:remainder :pair) nil))
             (:block ,slow nil
               ,@(float-divmod-code mode slow da db q r adj nil tmp unit
                                    (and (not need-rem) (member want '(:quotient :pair :fquotient)))
                                    (eq want :fquotient))
               ,@(ecase want
                   (:quotient
                    `((:local.get ,q) :f64.abs (:f64.const ,+f64-int-limit+) :f64.lt :i32.eqz (:hint 0) (:br_if ,slow)
                      ,@(integral-f64-i64-code q) (:local.tee ,w)
                      (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                      (:if nil :eqref ((:local.get ,w) :i32.wrap_i64 :ref.i31) ((:local.get ,w) (:call :i64-int)))))
                   (:fquotient `((:local.get ,q)))
                   (:pair `((:local.get ,q) :f64.abs (:f64.const ,+f64-int-limit+) :f64.lt :i32.eqz (:hint 0) (:br_if ,slow)
                            ,@(integral-f64-i64-code q)
                            (:local.set ,(cdr pair)) (:ref.null :eq) (:local.set ,(car pair))))
                   (:remainder nil))
               (:br ,done))
             ;; the helper decides (and signals)
             (:local.get ,da) (:struct.new :double) (:local.get ,db) (:struct.new :double)
             (:i32.const ,mode) (:call :divmod)
             ,@(ecase want
                 (:quotient nil)
                 (:fquotient '((:call :to-f64)))
                 (:pair `((:local.set ,(car pair))))
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
          ((and (member name '(sin cos tan atan exp log sinh cosh tanh asinh)) (= (length (cddr node)) 1)
                (ty-double-p (node-ty (third node))))
           ;; the host function; a result that is not finite is redone by
           ;; the helper, which signals
           (let ((x (new-local :f64)) (r (new-local :f64)))
             `(,@(cg-rep (third node) :f64) (:local.tee ,x) (:call ,(math-import name)) (:local.tee ,r)
               (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne
               (:if nil :f64
                    ((:local.get ,x) (:struct.new :double) (:call ,(math-helper name))
                     (:ref.cast :double) (:struct.get :double 0))
                    ((:local.get ,r))))))
          ((and (eq name 'atan) (= (length (cddr node)) 2) (float-divmod-node-p-args (cddr node)))
           `(,@(f64-arg-code (third node)) ,@(f64-arg-code (fourth node)) (:call :math-atan2)))
          ((and (eq name 'expt) (every (lambda (a) (ty-double-p (node-ty a))) (cddr node)))
           (let ((x (new-local :f64)) (y (new-local :f64)) (r (new-local :f64)) (slow (gensym "SLOW")) (done (gensym "EXPT")))
             `(,@(cg-rep (third node) :f64) (:local.set ,x) ,@(cg-rep (fourth node) :f64) (:local.set ,y)
               (:block ,done :f64
                 (:block ,slow nil
                   (:local.get ,x) (:f64.const 0) :f64.gt :i32.eqz (:br_if ,slow)
                   (:local.get ,x) (:local.get ,y) (:call :math-pow) (:local.tee ,r)
                   (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne (:br_if ,slow)
                   (:local.get ,r) (:br ,done))
                 (:local.get ,x) (:struct.new :double) (:local.get ,y) (:struct.new :double) (:call :fn-expt)
                 (:ref.cast :double) (:struct.get :double 0)))))
          (t nil))))

(defun float-divmod-node-p-args (args)
  (float-divmod-types-p (mapcar #'node-ty args)))

(defun numeric-i32-op-code-1 (node)
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
      (t nil))))

(defun numeric-i32-op-code (node)
  "i32 code for a numeric function whose result is within 32 bits, or NIL."
  (or (numeric-i32-op-code-1 node)
      ;; a result within 32 bits of operands within 64 (a mask, a right
      ;; shift, a remainder): the i64 operation, wrapped
      (let ((args (cddr node)))
        (when (and (ty-int32-p (node-ty node)) (some #'i64-arg-p args) (notevery #'i32-arg-p args))
          (let ((c (i64-op-code node))) (when c `(,@c :i32.wrap_i64)))))))

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

(defun i64-division-args-p (a b)
  "A and B are within 64 bits, B is not zero, and A / B cannot overflow."
  (and a b (i64-arg-p a) (i64-arg-p b)
       (let ((at (node-ty a)) (bt (node-ty b)))
         (and (or (> (ty-lo bt) 0) (< (ty-hi bt) 0))
              (or (> (ty-lo bt) 0) (> (ty-lo at) (- (expt 2 63))))))))

(defun numeric-i64-op-code (node)
  "i64 code for a numeric function whose result is within 64 bits, or NIL."
  (let* ((name (node-op-name node)) (args (cddr node)) (a (first args)) (b (second args)))
    (case name
      ((mod rem truncate floor)
       ;; MOD and FLOOR by a power of two: the low bits, a shift
       (let ((k (and (= (length args) 2) (member name '(mod floor)) (i64-arg-p a) (const-int-node b))))
         (when (and k (> k 0) (= (logcount k) 1) (< k (expt 2 63)))
           (return-from numeric-i64-op-code
             `(,@(cg-rep a :i64) ,@(cg-effect b)
               ,@(if (eq name 'mod)
                     `((:i64.const ,(1- k)) :i64.and)
                     `((:i64.const ,(1- (integer-length k))) :i64.shr_s))))))
       (when (and (= (length args) 2) (i64-division-args-p a b))
         (let ((x (new-local :i64)) (y (new-local :i64)) (q (new-local :i64)) (r (new-local :i64))
               (t1 (new-local :i64)) (t2 (new-local :i64)))
           `(,@(cg-rep a :i64) (:local.set ,x) ,@(cg-rep b :i64) (:local.set ,y)
             (:local.get ,x) (:local.get ,y) :i64.div_s (:local.set ,q)
             (:local.get ,x) (:local.get ,q) (:local.get ,y) :i64.mul :i64.sub (:local.set ,r)
             ,@(when (member name '(mod floor)) (int-divmod-adjust-code :i64 1 x y q r t1 t2))
             (:local.get ,(if (member name '(mod rem)) r q))))))
      (t nil))))

(defun numeric-divmod-code (vnode)
  "For MULTIPLE-VALUE-BIND of a division: code leaving the quotient (an
eqref), the local of the remainder and its representation; or NIL."
  ;; (floor a b) / (truncate a b) of integers within 64 bits
  (when (and (member (car vnode) '(:call :prim)) (member (node-op-name vnode) '(floor truncate))
             (= (length (cddr vnode)) 2) (i64-division-args-p (third vnode) (fourth vnode)))
    (let ((x (new-local :i64)) (y (new-local :i64)) (q (new-local :i64)) (r (new-local :i64))
          (t1 (new-local :i64)) (t2 (new-local :i64)))
      (return-from numeric-divmod-code
        (values
         `(,@(cg-rep (third vnode) :i64) (:local.set ,x) ,@(cg-rep (fourth vnode) :i64) (:local.set ,y)
           (:local.get ,x) (:local.get ,y) :i64.div_s (:local.set ,q)
           (:local.get ,x) (:local.get ,q) (:local.get ,y) :i64.mul :i64.sub (:local.set ,r)
           ,@(when (eq (node-op-name vnode) 'floor) (int-divmod-adjust-code :i64 1 x y q r t1 t2))
           (:local.get ,q) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
           (:if nil :eqref ((:local.get ,q) :i32.wrap_i64 :ref.i31) ((:local.get ,q) (:call :i64-int))))
         r :i64))))
  (multiple-value-bind (c rl) (spec-divmod-code vnode)
    (when c (return-from numeric-divmod-code (values c rl nil))))
  (when (float-divmod-node-p vnode)
    (let ((name (node-op-name vnode)))
      (cond ((member name '(floor truncate ceiling round))
             (multiple-value-bind (code r) (typed-float-divmod vnode :quotient t)
               (values code r :f64)))
            ((member name '(ffloor fceiling ftruncate fround))
             (multiple-value-bind (code r) (typed-float-divmod vnode :fquotient t)
               (values `(,@code (:struct.new :double)) r :f64)))))))

;;; A division of a value of unknown type (a sum of slots, a parameter
;;; declared INTEGER) by a constant fixnum, both values bound
;;; (MULTIPLE-VALUE-BIND of (FLOOR (+ sec offset) 86400)): tried in i64 (the
;;; dividend a fused tree, or decoded), the generic division otherwise.
(defun spec-divmod-code (vnode)
  (when (and (not *no-fuse*) (member (car vnode) '(:call :prim))
             (member (node-op-name vnode) '(floor truncate ceiling round))
             (= (length (cddr vnode)) 2))
    (let* ((a (third vnode)) (b (fourth vnode))
           (k (and (eq (car b) :const) (integerp (second b)) (second b)))
           (mode (position (node-op-name vnode) '(truncate floor ceiling round))))
      (when (and k (/= k 0) (/= k -1) (< (abs k) +char-tag+)
                 (logtest (ty-bits (node-ty a)) +k-int+) (not (ty-int64-p (node-ty a))))
        (let* ((x (new-local :i64)) (y (new-local :i64)) (q (new-local :i64)) (r (new-local :i64))
               (t1 (new-local :i64)) (t2 (new-local :i64)) (rl (new-local)) (e (new-local))
               (done (gensym "DIVQ")) (fail (gensym "DIVSLOW"))
               (tree (and (fusable-op-node-p a) (not (eq (fused-plan a nil) :no)))))
          (multiple-value-bind (leaves i64-code generic)
              (if tree
                  (fused-parts a)
                  (values `(,@(cg a) (:local.set ,e))
                          (lambda (fail) (i64-decode-code `((:local.get ,e)) fail))
                          `((:local.get ,e))))
            (values
                    `(,@leaves
                      (:block ,done :eqref
                        (:block ,fail nil
                          ,@(funcall i64-code fail) (:local.set ,x)
                          (:i64.const ,k) (:local.set ,y)
                          (:local.get ,x) (:local.get ,y) :i64.div_s (:local.set ,q)
                          (:local.get ,x) (:local.get ,y) :i64.rem_s (:local.set ,r)
                          ,@(int-divmod-adjust-code :i64 mode x y q r t1 t2)
                          ;; (the remainder is smaller than the divisor: a fixnum)
                          (:local.get ,r) :i32.wrap_i64 :ref.i31 (:local.set ,rl)
                          (:local.get ,q) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                          (:if nil :eqref ((:local.get ,q) :i32.wrap_i64 :ref.i31) ((:local.get ,q) (:call :i64-int)))
                          (:br ,done))
                        ,@generic ,@(cg b)
                        (:call ,(intern (format nil "DIVMOD~D" mode) :keyword))
                        (:global.get :mv-count) (:i32.const 1) :i32.gt_s
                        (:if nil :eqref ((:global.get :mv-buf) (:i32.const 0) (:array.get :vector)) ,(nil-code))
                        (:local.set ,rl)))
                    rl)))))))

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
(defvar *dual-uses-lam* nil "the function whose variables *DUAL-USES* describes")
(defvar *dual-uses* (make-hash-table :test 'eq)
  "lvar -> (arithmetic assignments . other uses) in the function being compiled")


(defun dual-uses (lv)
  "How LV is used in the function being compiled: a vector of the number of
its assignments from integer arithmetic, of its uses other than as an
operand of those, of those assignments within loops, of the uses within
loops other than as an operand of integer arithmetic or of a comparison
(which read the i64), and of the assignments within loops by arithmetic
with an operand known to be an integer.  (Each other use after an update
makes the integer, so the pair of locals pays only for a variable that is
updated more than it is looked at: a sum or a hash value, not a loop index.)"
  (let ((lam (fstate-lam *fs*)) (depth 0))
    (unless (eq lam *dual-uses-lam*)
      (clrhash *dual-uses*)
      (setf *dual-uses-lam* lam)
      (labels ((entry (v) (or (gethash v *dual-uses*) (setf (gethash v *dual-uses*) (vector 0 0 0 0 0))))
               (int-leaf-p (v n)
                 ;; the tree N has an operand other than V known to be an integer
                 (if (fusable-op-node-p n)
                     (some (lambda (a) (int-leaf-p v a)) (cddr n))
                     (and (not (and (eq (car n) :lref) (eq (second n) v)))
                          (if (eq (car n) :const)
                              (integerp (second n))
                              (ty-only-p (node-ty n) +k-int+)))))
               (ref (v leaf)
                 ;; LEAF: an operand of arithmetic or of a comparison
                 (let ((e (entry v)))
                   (incf (svref e 1))
                   (when (and (> depth 0) (not leaf)) (incf (svref e 3)))))
               (operands (v n)
                 ;; the references of V among the leaves of its update N are not other uses
                 (cond ((eq (car n) :lref) (unless (eq (second n) v) (ref (second n) t)))
                       ((fusable-op-node-p n) (dolist (a (cddr n)) (operands v a)))
                       (t (walk n))))
               (walk (n)
                 (when (consp n)
                   (case (car n)
                     (:lref (ref (second n) nil))
                     (:lset (let ((v (second n)) (e (third n)))
                              (if (and (eq (car e) :prim) (fusable-op-node-p e))
                                  (let ((en (entry v)))
                                    (incf (svref en 0))
                                    (when (> depth 0)
                                      (incf (svref en 2))
                                      (when (int-leaf-p v e) (incf (svref en 4))))
                                    (operands v e))
                                  (walk e))))
                     (:tagbody (incf depth) (map-subnodes #'walk n) (decf depth))
                     (t (if (or (fusable-op-node-p n) (spec-cmp-node-p n))
                            (dolist (a (cddr n)) (if (eq (car a) :lref) (ref (second a) t) (walk a)))
                            (map-subnodes #'walk n)))))))
        (walk (lam-body lam))))
    (or (gethash lv *dual-uses*) (vector 0 0 0 0 0))))

(defun note-dual-var (lv &optional param)
  "Called when LV gets its location (BIND-NEW-VAR-CODE, PARAM-SETUP-CODE).
A parameter's first value may be anything (its eqref local holds it): it is
an accumulator when its assignments are integer arithmetic."
  (if (and (lvar-mutated lv) (not (lvar-captured lv)) (not (lvar-special lv)) (null (lvar-rep lv))
           (let ((uses (dual-uses lv)) (ty (lvar-type lv)))
             (and (> (svref uses 0) 0)
                  (if (or param (and ty (ty-only-p ty +k-int+)))
                      (<= (svref uses 1) (svref uses 0))
                      ;; a number that may be an integer (what is not one
                      ;; goes to its eqref local), updated in a loop only by
                      ;; arithmetic with an operand known to be an integer,
                      ;; more often than it is looked at there (other than
                      ;; as an operand of arithmetic or of a comparison): a
                      ;; counter from a value of unknown type.  (Not one
                      ;; also updated by values of unknown types, whose
                      ;; updates are fixnums most often: the code of the
                      ;; i64 and f64 attempts of each, b-catch 36 -> 39 ms;
                      ;; nor one that may be something else, as the NIL a
                      ;; LOOP's FOR J = starts with: p-loop-macros 66 -> 78 ms.)
                      (and (or (null ty) (= (ty-bits ty) +k-all+) (and (logtest (ty-bits ty) +k-int+) (ty-only-p ty +k-number+)))
                           (> (svref uses 2) 0) (= (svref uses 4) (svref uses 2)) (< (svref uses 3) (svref uses 2)))))))
      (setf (gethash lv *dual-vars*) (new-local :i64))
      (remhash lv *dual-vars*)))

;;; Doubles of unknown type.  A LET variable bound to arithmetic that may give
;;; a double (a speculative tree, prims.lisp) and used as an operand of
;;; arithmetic has an f64 local beside its eqref local: when the tree took its
;;; fast path, the eqref local is null and the value is the f64; the first use
;;; that needs the object makes it (once) and keeps it in the eqref local.
(defvar *fdual-vars* (make-hash-table :test 'eq) "lvar -> index of its f64 local")

(defun fdual-local (lv)
  ;; (an entry of another compilation of the variable's function does not count:
  ;; the variable has its location in this one)
  (and (null (lvar-rep lv)) (not (boxed-p lv)) (gethash lv (fstate-locs *fs*)) (gethash lv *fdual-vars*)))

(defun fdual-materialize-code (b f)
  (let ((ok (gensym "FDUAL")))
    `((:block ,ok (:ref :eq)
        (:local.get ,b) (:br_on_non_null ,ok)
        (:local.get ,f) (:struct.new :double) (:local.tee ,b) :ref.as_non_null))))

(defun spec-cmp-node-p (node)
  (and (member (car node) '(:prim :call))
       (member (node-op-name node) '(< > <= >= = /=)) (= (length (cddr node)) 2)))

(defun arith-operand-use-p (lv body &optional (min 2))
  "LV is an operand of + - * / SQRT or a comparison MIN times or more in
BODY (an unboxed value made by arithmetic pays when it is used again: not
for the temporary of an INCF)."
  (let ((n 0))
    (labels ((walk (x)
               (when (consp x)
                 (when (or (spec-op-node-p x) (spec-cmp-node-p x))
                   (dolist (a (cddr x))
                     (when (and (eq (car a) :lref) (eq (second a) lv) (>= (incf n) min))
                       (return-from arith-operand-use-p t))))
                 ;; the value of a variable bound to it (the parameter of a
                 ;; copied function bound to the temporary of the argument)
                 (when (eq (car x) :let)
                   (dolist (b (second x))
                     (when (and (eq (cadr b) :lref) (eq (caddr b) lv) (not (lvar-mutated (car b)))
                                (arith-operand-use-p (car b) (third x) min))
                       (return-from arith-operand-use-p t))))
                 (map-subnodes #'walk x))))
      (walk body)
      nil)))

(defun fused-able-p (expr)
  "EXPR is + - * of integers or doubles that the fused trees compute (the
integer path first, then f64)."
  (and (spec-op-node-p expr) (member (prim-name (second expr)) '(+ - *))
       (not (eq (fused-plan expr nil) :no))))

(defun spec-able-p (expr)
  "EXPR may give a double, computed unboxed: a speculative tree with a double
among its operands, or arithmetic of the fused trees."
  (and (spec-op-node-p expr) (spec-op-p expr) (spec-tree-has-u-p expr)
       (or (nth-value 2 (spec-tree-stats expr)) (fused-able-p expr))))

(defun spec-store-code (expr b f)
  (if (nth-value 2 (spec-tree-stats expr))
      (spec-tree-code expr (cons b f))
      (fused-code expr :eqref (cons b f))))

(defun qdual-candidate-p (lv init)
  "LV, bound to INIT and never assigned, is the integer quotient of a
division of doubles computed inline: it has an i64 local beside its eqref
local (a DUAL-LOCAL, null eqref while the i64 holds it)."
  (and (not (lvar-captured lv)) (not (boxed-p lv)) (not (lvar-special lv)) (null (lvar-rep lv))
       (not (lvar-mutated lv)) (float-quotient-node-p init)))

(defun fdual-candidate-p (lv init body)
  "LV (bound to INIT around BODY) is kept as an unboxed double: it is an
operand of arithmetic, it may hold doubles, and its value or (for an
assigned variable, one that is not captured) an assignment is arithmetic
that may give a double."
  (and (not (lvar-captured lv)) (not (boxed-p lv)) (not (lvar-special lv)) (null (lvar-rep lv))
       (not *no-fuse*)
       (or (and (spec-able-p init)
                (or (not (lvar-mutated lv)) (nth-value 2 (spec-tree-stats init))))
           ;; a value that is a double on one of its paths, or another such variable
           (and (not (lvar-mutated lv)) (or (fdual-lref-p init) (dual-f64-node-p init)
                                            ;; (a double in a loop's version where the
                                            ;; guards of copies are taken as true)
                                            (ty-double-p (node-ty init))))
           (and (lvar-mutated lv) (lvar-type lv) (logtest (ty-bits (lvar-type lv)) +k-double+)
                (labels ((walk (n)
                           (when (consp n)
                             (when (and (eq (car n) :lset) (eq (second n) lv) (spec-able-p (third n))
                                        ;; (a double among the operands, or a
                                        ;; variable that starts as a double: not a counter)
                                        (or (nth-value 2 (spec-tree-stats (third n)))
                                            (ty-double-p (node-ty init))))
                               (return-from fdual-candidate-p
                                 ;; (an accumulator that starts as a double: one use is enough)
                                 (arith-operand-use-p lv body (if (ty-double-p (node-ty init)) 1 2))))
                             (map-subnodes #'walk n))))
                  (walk body)
                  nil)))
       ;; (a value that needs no arithmetic to be unboxed, or arithmetic
       ;; with a double among its operands: one use is enough)
       (arith-operand-use-p lv body (if (and (not (lvar-mutated lv))
                                             (or (fdual-lref-p init) (dual-f64-node-p init)
                                                 (ty-double-p (node-ty init))
                                                 (and (spec-able-p init) (nth-value 2 (spec-tree-stats init)))))
                                        1 2))))

(defun fdual-bind-code (lv init b f)
  (cond ((spec-able-p init) (spec-store-code init b f))
        ((or (fdual-lref-p init) (dual-f64-node-p init) (ty-double-p (node-ty init))) (cg-dual-f64 init b f))
        ((and (eq (car init) :const) (typep (second init) 'double-float))
         `((:f64.const ,(second init)) (:local.set ,f) (:ref.null :eq) (:local.set ,b)))
        (t `(,@(cg init) (:local.set ,b)))))

;;; Values that are doubles on some of their paths.  An IF (a LET, PROGN or
;;; BLOCK ending in one) one of whose branches gives a double or arithmetic
;;; that may give one, and the others anything (the guarded copy of a
;;; function defined in another form, whose other branch calls the function;
;;; (cond ((< x 0d0) 0d0) (t x))), is computed into a pair of locals as an
;;; unboxed double variable is (B null: the value is the f64 F), so that the
;;; double is not boxed where arithmetic or a double variable takes it.
(defun block-returned-to-p (blk body)
  (labels ((walk (n)
             (when (consp n)
               (when (and (eq (car n) :return-from) (eq (second n) blk))
                 (return-from block-returned-to-p t))
               (map-subnodes #'walk n))))
    (walk body)
    nil))

(defun fdual-lref-p (node)
  (and (eq (car node) :lref) (fdual-local (second node)) (eq (car (get-loc (second node))) :local)))

(defun dual-f64-branch-p (node)
  (or (ty-double-p (node-ty node)) (spec-able-p node) (fdual-lref-p node) (dual-f64-source-p node)))

(defun dual-f64-source-p (node)
  (case (car node)
    (:if (or (dual-f64-branch-p (third node)) (dual-f64-branch-p (fourth node))))
    (:progn (and (cdr node) (dual-f64-branch-p (car (last node)))))
    (:let (and (notany (lambda (b) (lvar-special (car b))) (second node))
               (not (or-let-p node))
               (dual-f64-branch-p (third node))))
    (:block (and (not (blk-nonlocal (second node)))
                 (dual-f64-branch-p (third node))
                 (not (block-returned-to-p (second node) (third node)))))
    (:the (dual-f64-branch-p (third node)))
    (t nil)))

(defun dual-f64-node-p (node)
  "NODE may give a double, not only doubles, and has a path giving one unboxed."
  (let ((ty (node-ty node)))
    (and (not *no-fuse*) (logtest (ty-bits ty) +k-double+) (not (ty-double-p ty))
         (dual-f64-source-p node))))

(defun cg-dual-f64 (node b f)
  "Code (leaving nothing) setting the pair of locals B (eqref) and F (f64) to
the value of NODE: B null when F holds it."
  (cond ((ty-double-p (node-ty node))
         `(,@(cg-rep node :f64) (:local.set ,f) (:ref.null :eq) (:local.set ,b)))
        ((spec-able-p node) (spec-store-code node b f))
        ((fdual-lref-p node)
         (let ((lv (second node)))
           `((:local.get ,(cdr (get-loc lv))) (:local.set ,b) (:local.get ,(fdual-local lv)) (:local.set ,f))))
        ((dual-f64-source-p node)
         (ecase (car node)
           (:if (if (assumed-guard-p (second node))
                    (cg-dual-f64 (third node) b f)
                `(,@(cg-test (second node))
                  (:if nil nil ,(cg-dual-f64 (third node) b f)
                       (,@(cg-dual-f64 (fourth node) b f) ,@(guard-else-epilogue (second node)))))))
           (:progn `(,@(loop for n in (butlast (cdr node)) append (cg-effect n))
                     ,@(cg-dual-f64 (car (last node)) b f)))
           (:let (cg-let node nil nil (list :dual b f)))
           ;; (no RETURN-FROM to it)
           (:block (cg-dual-f64 (third node) b f))
           (:the (cg-dual-f64 (third node) b f))))
        (t `(,@(cg node) (:local.set ,b)))))

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
    ;; an unboxed double variable
    (when (and (fdual-local lv) (eq (car (get-loc lv)) :local) (spec-able-p expr))
      (let ((b (cdr (get-loc lv))) (f (fdual-local lv)))
        (return-from dual-lset-code
          `(,@(spec-store-code expr b f)
            ,@(unless effect (fdual-materialize-code b f))))))
    (when (and v (eq (car (get-loc lv)) :local) (not *no-fuse*) (fusable-op-node-p expr)
               (not (eq (fused-plan expr nil) :no))
               ;; (a double among the operands: the arithmetic of doubles)
               (not (labels ((dbl (n) (if (fusable-op-node-p n)
                                          (some #'dbl (cddr n))
                                          (eq (spec-leaf-class n) :d))))
                      (dbl expr))))
      (let ((b (cdr (get-loc lv))) (fail (gensym "SLOW")) (fail2 (gensym "SLOWF")) (done (gensym "ACC")))
        (multiple-value-bind (leaves i64-code generic recs) (fused-parts expr)
          `(,@leaves
            (:block ,done nil
              (:block ,fail nil
                ,@(funcall i64-code fail)
                (:local.set ,v) (:ref.null :eq) (:local.set ,b)
                (:br ,done))
              ;; doubles (an accumulator that is not an integer)
              ,@(let ((code (let ((*fused-rest* recs)) (fused-f64-code expr fail2 t))))
                  (when code
                    `((:block ,fail2 nil ,@code (:struct.new :double) (:local.set ,b) (:br ,done)))))
              ,@generic
              (:local.set ,b))
            ,@(unless effect (dual-materialize-code b v))))))))
