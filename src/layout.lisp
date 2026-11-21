;;;; layout.lisp — WasmGC representation of Lisp objects
;;;;
;;;; Unitype: (ref null eq).  Immediates live in i31ref:
;;;;   fixnum     : i31 value in [-2^29, 2^29)
;;;;   character  : i31 value = 2^29 + code  (upper-bit tag "01")
;;;; Everything else is a heap struct/array, distinguished by RTT (ref.test).

(in-package :clysm)

(defconstant +fixnum-bits+ 30)
(defconstant +most-positive-fixnum+ (1- (expt 2 29)))
(defconstant +most-negative-fixnum+ (- (expt 2 29)))
(defconstant +char-tag+ (expt 2 29))
(defconstant +max-fixed-args+ 4)
(defconstant +max-req-layout-args+ 12)   ; (REQ-LAYOUT-OBJECT, src/codegen.lisp)
;; bignum magnitudes: little-endian arrays of base 2^32 digits (unsigned
;; in the i32 elements)
(defconstant +limb-bits+ 32)
(defconstant +limb-mask+ (1- (expt 2 32)))
(defconstant +mv-limit+ 1024)

;; struct field indices
(defconstant +sym-name+ 0) (defconstant +sym-package+ 1) (defconstant +sym-value+ 2)
(defconstant +sym-function+ 3) (defconstant +sym-plist+ 4) (defconstant +sym-hash+ 5)
(defconstant +sym-flags+ 6)
;; a closure: its code (shared by the closures of one lambda), its name,
;; and in a subtype the values of its free variables from field +CLO-ENV+ on
;; (:ENVCLOn for n = 1 to +CLO-ENV-MAX+ of them; :ENVCLOV holds more in a
;; vector, its field +CLO-ENV+)
(defconstant +clo-code+ 0) (defconstant +clo-name+ 1) (defconstant +clo-env+ 2)
(defconstant +clo-env-max+ 8)
;; the code: the entries for 0-4 arguments (0-4), for any number (5), the
;; function taking keyword arguments in fixed slots and its layout, the
;; entry taking a list of the arguments (APPLY)
(defconstant +clo-entry-n+ 5) (defconstant +clo-kmain+ 6) (defconstant +clo-klayout+ 7)
(defconstant +clo-entry-a+ 8)

;; symbol flags
(defconstant +flag-special+ 1)
(defconstant +flag-constant+ 2)

;;; Simple one-dimensional arrays are bare Wasm arrays ("storage").  Each kind
;;; of storage has a code (the index in this list), known to the runtime
;;; (runtime/seq.lisp *storage-element-types*):
;;;   (wasm-type element-type wasm-element-type lo hi)
;;; LO and HI bound the integer elements.
;;; The integer kinds are closed under intersection ((unsigned-byte 7) is
;;; what (unsigned-byte 8) and (signed-byte 8) share, and so on), so that the
;;; upgraded element type of a type is the smallest kind containing it.  They
;;; are listed smallest first.
(defparameter *storage-kinds*
  `((:vector t :eqref nil nil)
    (:string character :i32 nil nil)
    (:bits bit :i8 0 1)
    (:u7vec (unsigned-byte 7) :i8 0 127)
    (:bytes (unsigned-byte 8) :i8 0 255)
    (:s8vec (signed-byte 8) :i8 -128 127)
    (:u15vec (unsigned-byte 15) :i16 0 32767)
    (:u16vec (unsigned-byte 16) :i16 0 65535)
    (:s16vec (signed-byte 16) :i16 -32768 32767)
    (:u31vec (unsigned-byte 31) :i32 0 ,(1- (expt 2 31)))
    (:u32vec (unsigned-byte 32) :i32 0 ,(1- (expt 2 32)))
    (:s32vec (signed-byte 32) :i32 ,(- (expt 2 31)) ,(1- (expt 2 31)))
    (:u63vec (unsigned-byte 63) :i64 0 ,(1- (expt 2 63)))
    (:u64vec (unsigned-byte 64) :i64 0 ,(1- (expt 2 64)))
    (:s64vec (signed-byte 64) :i64 ,(- (expt 2 63)) ,(1- (expt 2 63)))
    (:f32vec single-float :f32 nil nil)
    (:f64vec double-float :f64 nil nil)))

(defun storage-kind-code (wasm-type) (position wasm-type *storage-kinds* :key #'first))
(defun storage-kind-info (wasm-type) (assoc wasm-type *storage-kinds*))

(defun storage-kind-of-element-type (et)
  "The storage (wasm type name) of arrays whose element type is the type
specifier ET, as UPGRADED-ARRAY-ELEMENT-TYPE (runtime/seq.lisp) decides; NIL
when ET is not one of the specifiers recognized at compile time."
  (flet ((int-kind (lo hi)
           (cond ((not (and (integerp lo) (integerp hi))) :vector)
                 ((< hi lo) nil)
                 (t (or (loop for (name nil nil klo khi) in *storage-kinds*
                              when (and klo (<= klo lo) (<= hi khi)) return name)
                        :vector)))))
    (cond ((eq et t) :vector)
          ((member et '(character base-char standard-char)) :string)
          ((eq et 'bit) :bits)
          ((member et '(double-float long-float)) :f64vec)
          ((member et '(single-float short-float)) :f32vec)
          ((eq et 'fixnum) (int-kind +most-negative-fixnum+ +most-positive-fixnum+))
          ((member et '(integer symbol number real float rational ratio complex cons list atom
                        sequence function unsigned-byte signed-byte))
           :vector)
          ((and (consp et) (eq (car et) 'unsigned-byte) (integerp (cadr et)) (plusp (cadr et)) (null (cddr et)))
           (int-kind 0 (1- (expt 2 (cadr et)))))
          ((and (consp et) (eq (car et) 'signed-byte) (integerp (cadr et)) (plusp (cadr et)) (null (cddr et)))
           (int-kind (- (expt 2 (1- (cadr et)))) (1- (expt 2 (1- (cadr et))))))
          ((and (consp et) (eq (car et) 'integer) (consp (cdr et)) (consp (cddr et)) (null (cdddr et)))
           (let ((lo (cadr et)) (hi (caddr et)))
             (int-kind (if (and (consp lo) (integerp (car lo))) (1+ (car lo)) lo)
                       (if (and (consp hi) (integerp (car hi))) (1- (car hi)) hi))))
          ((and (consp et) (eq (car et) 'mod) (integerp (cadr et)) (plusp (cadr et)) (null (cddr et)))
           (int-kind 0 (1- (cadr et))))
          ;; a type with objects that are no numbers or characters (an
          ;; array, a structure, (OR (EQL 0) CHUNK)): only T holds them
          ((or (element-type-needs-t-p et)
               (and (consp et) (eq (car et) 'or) (some #'element-type-needs-t-p (cdr et))))
           :vector)
          (t nil))))

(defun element-type-needs-t-p (et)
  "ET (a type specifier) has elements that are no numbers or characters, so
that UPGRADED-ARRAY-ELEMENT-TYPE makes it T."
  (let ((et (ignore-errors (expand-deftypes et))))
    (cond ((symbolp et)
           (or (member et '(t symbol null keyword cons list function sequence atom string simple-string
                            base-string simple-base-string vector simple-vector array simple-array
                            bit-vector simple-bit-vector hash-table stream package pathname
                            structure-object standard-object))
               (struct-class-type-p et)))
          ((consp et)
           (case (car et)
             ((simple-array array vector simple-vector string simple-string base-string simple-base-string
               cons function bit-vector simple-bit-vector)
              t)
             ((member eql) (some (lambda (x) (not (or (numberp x) (characterp x)))) (cdr et)))
             (t nil))))))

(defun envclo-type (n)
  "The type of a closure with N (> 0) free variables."
  (if (<= n +clo-env-max+)
      (intern (format nil "ENVCLO~D" n) :keyword)
      :envclov))

(defparameter *type-defs*
  `((:string    (:array (:i32 t)))
    (:vector    (:array (:eqref t)))
    ;; the binding stack (its top and the vector of its entries), which the
    ;; image and the modules compiled in it reach through one immutable
    ;; global (an imported mutable global costs a load more at each access)
    (:dynstate  (:struct ((:i32 t) ((:ref :vector) t))))
    (:limbs     (:array (:i32 t)))
    (:bytes     (:array (:i8 t)))
    (:bits      (:array (:i8 t)))
    (:f64vec    (:array (:f64 t)))
    (:s8vec     (:array (:i8 t)))
    (:u7vec     (:array (:i8 t)))
    (:u15vec    (:array (:i16 t)))
    (:u31vec    (:array (:i32 t)))
    (:u63vec    (:array (:i64 t)))
    (:u16vec    (:array (:i16 t)))
    (:s16vec    (:array (:i16 t)))
    (:u32vec    (:array (:i32 t)))
    (:s32vec    (:array (:i32 t)))
    (:u64vec    (:array (:i64 t)))
    (:s64vec    (:array (:i64 t)))
    (:f32vec    (:array (:f32 t)))
    (:cons      (:struct ((:eqref t) (:eqref t))))
    (:package   (:struct ((:eqref t) (:eqref t) (:eqref t) (:eqref t) (:eqref t) (:eqref t) (:eqref t) (:eqref t))))
    (:symbol    (:struct (((:ref :string) nil) ((:ref :null :package) t) (:eqref t)
                          ((:ref :null :closure) t) (:eqref t) (:i32 nil) (:i32 t))))
    (:closure   (:sub () nil (:struct (((:ref :code) nil) (:eqref t)))))
    (:code      (:struct (((:ref :f0) nil) ((:ref :f1) nil) ((:ref :f2) nil) ((:ref :f3) nil)
                          ((:ref :f4) nil) ((:ref :fN) nil)
                          ;; for a function with keyword parameters: the function taking them
                          ;; in fixed slots, and its layout (required optional . keywords)
                          ((:ref :null :func) nil) (:eqref nil)
                          ;; the entry taking the arguments as a list
                          ((:ref :fA) nil))))
    (:f0        (:func ((:ref :closure)) (:eqref)))
    (:f1        (:func ((:ref :closure) :eqref) (:eqref)))
    (:f2        (:func ((:ref :closure) :eqref :eqref) (:eqref)))
    (:f3        (:func ((:ref :closure) :eqref :eqref :eqref) (:eqref)))
    (:f4        (:func ((:ref :closure) :eqref :eqref :eqref :eqref) (:eqref)))
    (:fN        (:func ((:ref :closure) (:ref :vector)) (:eqref)))
    (:fA        (:func ((:ref :closure) :eqref) (:eqref)))
    ,@(loop for n from 1 to +clo-env-max+
            collect `(,(envclo-type n)
                      (:sub (:closure) t (:struct (((:ref :code) nil) (:eqref t)
                                                   ,@(loop repeat n collect '(:eqref t)))))))
    (:envclov   (:sub (:closure) t (:struct (((:ref :code) nil) (:eqref t) ((:ref :vector) nil)))))
    (:cell      (:struct ((:eqref t))))
    (:bignum    (:struct ((:i32 nil) ((:ref :null :limbs) t) (:i64 nil))))  ; sign limbs value (limbs null: |value| < 2^63 in VALUE)
    (:ratio     (:struct ((:eqref nil) (:eqref nil))))
    (:single    (:struct ((:f32 nil))))
    (:double    (:struct ((:f64 nil))))
    (:complex   (:struct ((:eqref nil) (:eqref nil))))
    ;; complex numbers whose parts are double floats / single floats
    (:complex-d (:struct ((:f64 nil) (:f64 nil))))
    (:complex-s (:struct ((:f32 nil) (:f32 nil))))
    ;; layout slots hash; not final: structures are its subtypes (below)
    (:instance  (:sub () nil (:struct ((:eqref t) ((:ref :vector) t) (:i32 t)))))
    ;; data fill dims offset info, the two dimensions of a rank-2 array (else 0 0),
    ;; and the dimension of a one-dimensional array (else 0)
    (:mdarray   (:struct ((:eqref t) (:eqref t) (:eqref t) (:eqref t) (:eqref t) (:i32 t) (:i32 t) (:i32 t))))
    (:foreign   (:struct ((:externref nil))))
    ;; the octets of a file stream, its position and the number of valid
    ;; octets (runtime/stream.lisp); never seen by Lisp code
    (:fsbuf     (:struct (((:ref :null :bytes) t) (:i32 t) (:i32 t))))
    ;; misc function types
    (:exn       (:func (:eqref :eqref) ()))
    (:bt-catch  (:func () (:eqref :eqref :exnref)))
    (:v-v       (:func () ()))
    (:v-e       (:func () (:eqref)))
    (:e-v       (:func (:eqref) ()))
    (:e-e       (:func (:eqref) (:eqref)))
    (:ee-e      (:func (:eqref :eqref) (:eqref)))
    (:eee-e     (:func (:eqref :eqref :eqref) (:eqref)))
    (:eeee-e    (:func (:eqref :eqref :eqref :eqref) (:eqref)))
    (:ee-v      (:func (:eqref :eqref) ()))
    (:e-i       (:func (:eqref) (:i32)))
    (:ee-i      (:func (:eqref :eqref) (:i32)))
    (:i-v       (:func (:i32) ()))
    (:v-i       (:func () (:i32)))
    (:i-i       (:func (:i32) (:i32)))
    (:ii-v      (:func (:i32 :i32) ()))
    (:ii-i      (:func (:i32 :i32) (:i32)))
    (:i-e       (:func (:i32) (:eqref)))
    (:d-d       (:func (:f64) (:f64)))
    (:dd-d      (:func (:f64 :f64) (:f64)))
    (:v-d       (:func () (:f64)))
    (:di-i      (:func (:f64 :i32) (:i32)))
    (:x-i       (:func (:externref) (:i32)))
    (:sym-clo   (:func ((:ref :symbol)) ((:ref :closure))))
    (:e-clo     (:func (:eqref) ((:ref :closure))))
    (:ev-e      (:func (:eqref (:ref :vector)) (:eqref)))
    (:l-v       (:func (:eqref) ((:ref :vector))))
    (:vi-e      (:func ((:ref :vector) :i32) (:eqref)))
    (:vie-e     (:func ((:ref :vector) :i32 :eqref) (:eqref)))
    (:vie-v     (:func ((:ref :vector) :i32 :eqref) ()))
    (:e-cons    (:func (:eqref) ((:ref :cons))))
    (:e-vec     (:func (:eqref) ((:ref :vector))))
    (:e-sym     (:func (:eqref) ((:ref :symbol))))
    (:e-pkgn    (:func (:eqref) ((:ref :null :package))))
    (:sym-e     (:func ((:ref :symbol)) (:eqref)))
    (:e-d       (:func (:eqref) (:f64)))
    (:di-d      (:func (:f64 :i32) (:f64)))
    (:ddi-d     (:func (:f64 :f64 :i32) (:f64)))
    (:i-d       (:func (:i32) (:f64)))
    (:v-x       (:func () (:externref)))
    (:x-x       (:func (:externref) (:externref)))
    (:xx-x      (:func (:externref :externref) (:externref)))
    (:xxx-x     (:func (:externref :externref :externref) (:externref)))
    (:xxx-v     (:func (:externref :externref :externref) ()))
    (:xx-v      (:func (:externref :externref) ()))
    (:d-x       (:func (:f64) (:externref)))
    (:x-d       (:func (:externref) (:f64)))
    (:xi-i      (:func (:externref :i32) (:i32)))
    (:i-x       (:func (:i32) (:externref)))
    (:x-e       (:func (:externref) (:eqref)))
    ;; the RUN function of a module compiled in the image (host instantiate_module)
    (:x-run     (:func (:externref) ((:ref :null :x-e))))
    (:e-l       (:func (:eqref) (:i64)))
    (:l-e       (:func (:i64) (:eqref)))
    (:eei-e     (:func (:eqref :eqref :i32) (:eqref)))
    (:eeiii-e   (:func (:eqref :eqref :i32 :i32 :i32) (:eqref)))
    (:ei-e      (:func (:eqref :i32) (:eqref)))
    (:v-dd      (:func () (:f64 :f64)))
    (:v-ff      (:func () (:f32 :f32)))
    (:eie-i     (:func (:eqref :i32 :eqref) (:i32)))
    (:eie-e     (:func (:eqref :i32 :eqref) (:eqref)))
    (:ii-e      (:func (:i32 :i32) (:eqref)))
    (:eeii-i    (:func (:eqref :eqref :i32 :i32) (:i32)))
    (:eieii-i   (:func (:eqref :i32 :eqref :i32 :i32) (:i32)))
    ;; numeric helpers (prims.lisp)
    (:d-e       (:func (:f64) (:eqref)))
    (:ll-e      (:func (:i64 :i64) (:eqref)))
    (:eii-e     (:func (:eqref :i32 :i32) (:eqref)))
    (:ee-d      (:func (:eqref :eqref) (:f64)))
    (:eeei-e    (:func (:eqref :eqref :eqref :i32) (:eqref)))
    (:ll-l      (:func (:i64 :i64) (:i64)))
    (:eli-e     (:func (:eqref :i64 :i32) (:eqref)))
    (:eeeee-e   (:func (:eqref :eqref :eqref :eqref :eqref) (:eqref)))
    ;; heap images: a function's index in the table of functions
    (:fr-i      (:func ((:ref :null :func)) (:i32)))
    (:v-l       (:func () (:i64)))))

;;; Structures (DEFSTRUCT, without :TYPE) of 1 to +STRUCT-INLINE-MAX+ slots
;;; keep their slots in fields of their own: a structure of K slots is a
;;; $siK, which is the $instance fields (layout, an empty slot vector, hash)
;;; then K mutable eqref fields.  Each $siK is a subtype of $si(K-1), so slot
;;; I of any structure with more than I slots is read with a cast to $si(I+1)
;;; and a struct.get (an included structure's slots come first).  Larger
;;; structures keep a slot vector, as other instances.
(defconstant +struct-inline-max+ 32)
(defun sinst-type (k) (intern (format nil "SI~D" k) :keyword))
(defun sinst-field (i) (+ 3 i))
;; $sfield: any structure whose slots are fields (the $siK, and the types of
;; structures with slots of a number type, below)
(setf *type-defs*
      (append *type-defs*
              '((:sfield (:sub (:instance) nil (:struct ((:eqref t) ((:ref :vector) t) (:i32 t))))))
              (loop for k from 1 to +struct-inline-max+
                    collect (list (sinst-type k)
                                  `(:sub (,(if (= k 1) :sfield (sinst-type (1- k)))) ,(= k +struct-inline-max+)
                                         (:struct ((:eqref t) ((:ref :vector) t) (:i32 t)
                                                   ,@(loop repeat k collect '(:eqref t)))))))))

(defun fixed-entry-type (n) (ecase n (0 :f0) (1 :f1) (2 :f2) (3 :f3) (4 :f4)))
