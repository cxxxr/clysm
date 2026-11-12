;;;; wasm.lisp — WebAssembly binary encoder (GC, EH(exnref), tail calls)
;;;;
;;;; A module is described symbolically: types, functions, globals and tags are
;;;; referred to by name (any EQL key), and labels in structured instructions are
;;;; symbolic and resolved to relative depths at encoding time.

(in-package :clysm-wasm)

;;; ------------------------------------------------------------
;;; Byte buffers / LEB128
;;; ------------------------------------------------------------

(defun make-buf () (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
(declaim (inline emit-byte))
(defun emit-byte (buf b) (vector-push-extend (ldb (byte 8 0) b) buf))
(defun emit-bytes (buf bytes) (loop for b across bytes do (vector-push-extend b buf)))

(defun emit-u32 (buf n)
  (assert (>= n 0))
  (loop
    (let ((b (ldb (byte 7 0) n)))
      (setf n (ash n -7))
      (if (zerop n)
          (progn (emit-byte buf b) (return))
          (emit-byte buf (logior b #x80))))))

(defun emit-sleb (buf n)
  (loop
    (let ((b (ldb (byte 7 0) n)))
      (setf n (ash n -7))
      (if (or (and (zerop n) (zerop (logand b #x40)))
              (and (= n -1) (not (zerop (logand b #x40)))))
          (progn (emit-byte buf b) (return))
          (emit-byte buf (logior b #x80))))))

(defun utf8-octets (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for c = (char-code ch)
          do (cond ((< c #x80) (vector-push-extend c out))
                   ((< c #x800)
                    (vector-push-extend (logior #xC0 (ash c -6)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   ((< c #x10000)
                    (vector-push-extend (logior #xE0 (ash c -12)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   (t
                    (vector-push-extend (logior #xF0 (ash c -18)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -12) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))))
    out))

(defun emit-name (buf string)
  (let ((octets (utf8-octets string)))
    (emit-u32 buf (length octets))
    (emit-bytes buf octets)))

(defun float-bits (x mantissa-bits exponent-bits)
  "IEEE 754 bit pattern of the float X, computed portably."
  (let ((bias (1- (ash 1 (1- exponent-bits))))
        (sign-bit (ash 1 (+ mantissa-bits exponent-bits))))
    (if (zerop x)
        (if (minusp (float-sign x)) sign-bit 0)
        (multiple-value-bind (mant expo sign) (integer-decode-float x)
          ;; normalize mantissa to MANTISSA-BITS+1 bits
          (let ((len (integer-length mant)))
            (setf expo (+ expo (- len (1+ mantissa-bits))))
            (setf mant (ash mant (- (1+ mantissa-bits) len))))
          (let ((biased (+ expo mantissa-bits bias)))
            (when (<= biased 0)
              ;; subnormal
              (setf mant (ash mant (1- biased)) biased 0))
            (logior (if (minusp sign) sign-bit 0)
                    (ash biased mantissa-bits)
                    (ldb (byte mantissa-bits 0) mant)))))))

(defun emit-vec-buf (buf sub)
  (emit-u32 buf (length sub))
  (emit-bytes buf sub))

;;; ------------------------------------------------------------
;;; Module model
;;; ------------------------------------------------------------

(defstruct (wasm-func (:constructor %make-wasm-func))
  name type params-names locals local-names body export import-module import-name display)

(defstruct wasm-module
  (types (make-array 0 :adjustable t :fill-pointer 0))   ; vector of (name . def)
  (type-table (make-hash-table :test 'equal))
  (imports (make-array 0 :adjustable t :fill-pointer 0)) ; imported funcs (wasm-func)
  (funcs (make-array 0 :adjustable t :fill-pointer 0))   ; defined funcs
  (func-table (make-hash-table :test 'equal))            ; name -> wasm-func
  (func-index-table (make-hash-table :test 'equal))
  (globals (make-array 0 :adjustable t :fill-pointer 0)) ; (name type mutable init)
  (global-table (make-hash-table :test 'equal))
  (tags (make-array 0 :adjustable t :fill-pointer 0))    ; (name type)
  (tag-table (make-hash-table :test 'equal))
  (exports nil)
  (data (make-array 0 :adjustable t :fill-pointer 0))    ; passive data segments (octet vectors)
  (declared-refs (make-hash-table :test 'equal))
  (start nil)
  (frozen nil)
  (base-types nil)                                       ; size of the first rec group
  (global-imports (make-array 0 :adjustable t :fill-pointer 0)) ; (name type mutable module field)
  (tag-imports (make-array 0 :adjustable t :fill-pointer 0))    ; (name type module field)
  (nonnull-globals (make-hash-table :test 'equal))
  (cast-rewrites nil))                                  ; heap type -> function replacing ref.cast     ; nullable globals read as non-null

(defun add-type (m name def)
  "DEF is (:func params results) | (:struct fields) | (:array field) optionally
wrapped as (:sub supers final-p def). Field = (storage-type mutable-p)."
  (let ((idx (fill-pointer (wasm-module-types m))))
    (vector-push-extend (cons name def) (wasm-module-types m))
    (setf (gethash name (wasm-module-type-table m)) idx)
    idx))

(defun type-index (m name)
  (if (integerp name) name
      (or (gethash name (wasm-module-type-table m))
          (error "wasm: unknown type ~S" name))))

(defun add-import-func (m name type module field)
  (assert (zerop (fill-pointer (wasm-module-funcs m))) () "imports must precede functions")
  (let ((f (%make-wasm-func :name name :type type :import-module module :import-name field)))
    (setf (gethash name (wasm-module-func-index-table m)) (fill-pointer (wasm-module-imports m)))
    (vector-push-extend f (wasm-module-imports m))
    (setf (gethash name (wasm-module-func-table m)) f)
    f))

(defun add-func (m name type &key locals local-names body export param-names display)
  (let ((f (%make-wasm-func :name name :type type :locals locals :local-names local-names :display display
                            :params-names param-names :body body :export export)))
    (setf (gethash name (wasm-module-func-index-table m))
          (+ (fill-pointer (wasm-module-imports m)) (fill-pointer (wasm-module-funcs m))))
    (vector-push-extend f (wasm-module-funcs m))
    (setf (gethash name (wasm-module-func-table m)) f)
    (when export (add-export m export :func name))
    f))

(defun find-func (m name) (gethash name (wasm-module-func-table m)))
(defun func-defined-p (m name) (nth-value 1 (gethash name (wasm-module-func-table m))))

(defun func-index (m name)
  (or (gethash name (wasm-module-func-index-table m))
      (error "wasm: unknown function ~S" name)))

(defun declare-func-ref (m name)
  (setf (gethash name (wasm-module-declared-refs m)) t))

(defun add-import-global (m name type mutable module field)
  (let ((idx (fill-pointer (wasm-module-global-imports m))))
    (vector-push-extend (list name type mutable module field) (wasm-module-global-imports m))
    (setf (gethash name (wasm-module-global-table m)) idx)
    idx))

(defun add-import-tag (m name type module field)
  (let ((idx (fill-pointer (wasm-module-tag-imports m))))
    (vector-push-extend (list name type module field) (wasm-module-tag-imports m))
    (setf (gethash name (wasm-module-tag-table m)) idx)
    idx))

(defun add-global (m name type mutable init)
  (let ((idx (+ (fill-pointer (wasm-module-global-imports m)) (fill-pointer (wasm-module-globals m)))))
    (vector-push-extend (list name type mutable init) (wasm-module-globals m))
    (setf (gethash name (wasm-module-global-table m)) idx)
    idx))

(defun global-defined-p (m name) (nth-value 1 (gethash name (wasm-module-global-table m))))

(defun global-index (m name)
  (or (gethash name (wasm-module-global-table m))
      (error "wasm: unknown global ~S" name)))

(defun add-tag (m name type)
  (let ((idx (+ (fill-pointer (wasm-module-tag-imports m)) (fill-pointer (wasm-module-tags m)))))
    (vector-push-extend (list name type) (wasm-module-tags m))
    (setf (gethash name (wasm-module-tag-table m)) idx)
    idx))

(defun tag-index (m name)
  (or (gethash name (wasm-module-tag-table m)) (error "wasm: unknown tag ~S" name)))

(defun add-export (m name kind ref)
  (push (list name kind ref) (wasm-module-exports m)))

(defun add-data (m octets)
  (let ((idx (fill-pointer (wasm-module-data m))))
    (vector-push-extend octets (wasm-module-data m))
    idx))

;;; ------------------------------------------------------------
;;; Types
;;; ------------------------------------------------------------

(defparameter *abs-heap-types*
  '((:nofunc . #x73) (:noextern . #x72) (:none . #x71) (:func . #x70) (:extern . #x6F)
    (:any . #x6E) (:eq . #x6D) (:i31 . #x6C) (:struct . #x6B) (:array . #x6A)
    (:exn . #x69) (:noexn . #x74)))

(defparameter *shorthand-reftypes*
  '((:funcref . #x70) (:externref . #x6F) (:anyref . #x6E) (:eqref . #x6D)
    (:i31ref . #x6C) (:structref . #x6B) (:arrayref . #x6A) (:exnref . #x69)
    (:nullref . #x71) (:nullfuncref . #x73) (:nullexternref . #x72)))

(defun emit-heap-type (m buf ht)
  (let ((abs (assoc ht *abs-heap-types*)))
    (if abs
        (emit-byte buf (cdr abs))
        (emit-sleb buf (type-index m ht)))))

(defun emit-valtype (m buf vt)
  (cond ((eq vt :i32) (emit-byte buf #x7F))
        ((eq vt :i64) (emit-byte buf #x7E))
        ((eq vt :f32) (emit-byte buf #x7D))
        ((eq vt :f64) (emit-byte buf #x7C))
        ((eq vt :v128) (emit-byte buf #x7B))
        ((assoc vt *shorthand-reftypes*) (emit-byte buf (cdr (assoc vt *shorthand-reftypes*))))
        ((and (consp vt) (eq (first vt) :ref))
         ;; (:ref ht) or (:ref :null ht)
         (if (eq (second vt) :null)
             (progn (emit-byte buf #x63) (emit-heap-type m buf (third vt)))
             (progn (emit-byte buf #x64) (emit-heap-type m buf (second vt)))))
        (t (error "wasm: bad valtype ~S" vt))))

(defun emit-storage-type (m buf st)
  (case st
    (:i8 (emit-byte buf #x78))
    (:i16 (emit-byte buf #x77))
    (t (emit-valtype m buf st))))

(defun emit-field (m buf field)
  ;; FIELD is (storage-type mutable-p)
  (destructuring-bind (st &optional mut) field
    (emit-storage-type m buf st)
    (emit-byte buf (if mut 1 0))))

(defun emit-comptype (m buf def)
  (ecase (first def)
    (:func
     (destructuring-bind (params results) (rest def)
       (emit-byte buf #x60)
       (emit-u32 buf (length params))
       (dolist (p params) (emit-valtype m buf p))
       (emit-u32 buf (length results))
       (dolist (r results) (emit-valtype m buf r))))
    (:struct
     (emit-byte buf #x5F)
     (let ((fields (second def)))
       (emit-u32 buf (length fields))
       (dolist (f fields) (emit-field m buf f))))
    (:array
     (emit-byte buf #x5E)
     (emit-field m buf (second def)))))

(defun emit-subtype (m buf def)
  (if (eq (first def) :sub)
      (destructuring-bind (supers final-p comp) (rest def)
        (emit-byte buf (if final-p #x4F #x50))
        (emit-u32 buf (length supers))
        (dolist (s supers) (emit-u32 buf (type-index m s)))
        (emit-comptype m buf comp))
      (emit-comptype m buf def)))

;;; ------------------------------------------------------------
;;; Instructions
;;; ------------------------------------------------------------

(defparameter *simple-ops* (make-hash-table))
(defparameter *gc-ops* (make-hash-table))
(defparameter *fc-ops* (make-hash-table))

(macrolet ((simple (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *simple-ops*) ,code)))))
  (simple :unreachable #x00 :nop #x01 :return #x0F :drop #x1A :select #x1B
          :throw_ref #x0A
          :ref.is_null #xD1 :ref.eq #xD3 :ref.as_non_null #xD4
          :i32.eqz #x45 :i32.eq #x46 :i32.ne #x47 :i32.lt_s #x48 :i32.lt_u #x49
          :i32.gt_s #x4A :i32.gt_u #x4B :i32.le_s #x4C :i32.le_u #x4D :i32.ge_s #x4E :i32.ge_u #x4F
          :i64.eqz #x50 :i64.eq #x51 :i64.ne #x52 :i64.lt_s #x53 :i64.lt_u #x54 :i64.gt_s #x55
          :i64.gt_u #x56 :i64.le_s #x57 :i64.le_u #x58 :i64.ge_s #x59 :i64.ge_u #x5A
          :f32.eq #x5B :f32.ne #x5C :f32.lt #x5D :f32.gt #x5E :f32.le #x5F :f32.ge #x60
          :f64.eq #x61 :f64.ne #x62 :f64.lt #x63 :f64.gt #x64 :f64.le #x65 :f64.ge #x66
          :i32.clz #x67 :i32.ctz #x68 :i32.popcnt #x69 :i32.add #x6A :i32.sub #x6B :i32.mul #x6C
          :i32.div_s #x6D :i32.div_u #x6E :i32.rem_s #x6F :i32.rem_u #x70 :i32.and #x71
          :i32.or #x72 :i32.xor #x73 :i32.shl #x74 :i32.shr_s #x75 :i32.shr_u #x76
          :i32.rotl #x77 :i32.rotr #x78
          :i64.clz #x79 :i64.ctz #x7A :i64.popcnt #x7B :i64.add #x7C :i64.sub #x7D :i64.mul #x7E
          :i64.div_s #x7F :i64.div_u #x80 :i64.rem_s #x81 :i64.rem_u #x82 :i64.and #x83
          :i64.or #x84 :i64.xor #x85 :i64.shl #x86 :i64.shr_s #x87 :i64.shr_u #x88
          :i64.rotl #x89 :i64.rotr #x8A
          :f32.abs #x8B :f32.neg #x8C :f32.ceil #x8D :f32.floor #x8E :f32.trunc #x8F
          :f32.nearest #x90 :f32.sqrt #x91 :f32.add #x92 :f32.sub #x93 :f32.mul #x94
          :f32.div #x95 :f32.min #x96 :f32.max #x97 :f32.copysign #x98
          :f64.abs #x99 :f64.neg #x9A :f64.ceil #x9B :f64.floor #x9C :f64.trunc #x9D
          :f64.nearest #x9E :f64.sqrt #x9F :f64.add #xA0 :f64.sub #xA1 :f64.mul #xA2
          :f64.div #xA3 :f64.min #xA4 :f64.max #xA5 :f64.copysign #xA6
          :i32.wrap_i64 #xA7 :i32.trunc_f32_s #xA8 :i32.trunc_f32_u #xA9
          :i32.trunc_f64_s #xAA :i32.trunc_f64_u #xAB
          :i64.extend_i32_s #xAC :i64.extend_i32_u #xAD :i64.trunc_f32_s #xAE :i64.trunc_f32_u #xAF
          :i64.trunc_f64_s #xB0 :i64.trunc_f64_u #xB1
          :f32.convert_i32_s #xB2 :f32.convert_i32_u #xB3 :f32.convert_i64_s #xB4
          :f32.convert_i64_u #xB5 :f32.demote_f64 #xB6
          :f64.convert_i32_s #xB7 :f64.convert_i32_u #xB8 :f64.convert_i64_s #xB9
          :f64.convert_i64_u #xBA :f64.promote_f32 #xBB
          :i32.reinterpret_f32 #xBC :i64.reinterpret_f64 #xBD :f32.reinterpret_i32 #xBE
          :f64.reinterpret_i64 #xBF
          :i32.extend8_s #xC0 :i32.extend16_s #xC1 :i64.extend8_s #xC2 :i64.extend16_s #xC3
          :i64.extend32_s #xC4))

(macrolet ((gc (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *gc-ops*) ,code)))))
  ;; ops without immediates
  (gc :array.len 15 :any.convert_extern 26 :extern.convert_any 27 :ref.i31 28
      :i31.get_s 29 :i31.get_u 30))

(macrolet ((fc (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *fc-ops*) ,code)))))
  (fc :i32.trunc_sat_f32_s 0 :i32.trunc_sat_f32_u 1 :i32.trunc_sat_f64_s 2 :i32.trunc_sat_f64_u 3
      :i64.trunc_sat_f32_s 4 :i64.trunc_sat_f32_u 5 :i64.trunc_sat_f64_s 6 :i64.trunc_sat_f64_u 7))

(defvar *labels*)
(defvar *current-func* nil)
(defvar *module*)
(defvar *locals-map*)

(defun label-depth (label)
  (if (integerp label) label
      (let ((pos (position label *labels*)))
        (unless pos (error "wasm: unknown label ~S (in scope: ~S)" label *labels*))
        pos)))

(defun emit-blocktype (buf bt)
  (cond ((null bt) (emit-byte buf #x40))
        ((and (consp bt) (eq (first bt) :type)) (emit-sleb buf (type-index *module* (second bt))))
        (t (emit-valtype *module* buf bt))))

(defun local-index (x)
  (if (integerp x) x
      (or (gethash x *locals-map*) (error "wasm: unknown local ~S" x))))

(defun emit-instr (buf ins)
  (let ((m *module*))
    (when (keywordp ins) (setf ins (list ins)))
    (let ((op (first ins)) (args (rest ins)))
      (multiple-value-bind (code found) (gethash op *simple-ops*)
        (when found
          (emit-byte buf code)
          (return-from emit-instr)))
      (multiple-value-bind (code found) (gethash op *gc-ops*)
        (when found
          (emit-byte buf #xFB) (emit-u32 buf code)
          (return-from emit-instr)))
      (multiple-value-bind (code found) (gethash op *fc-ops*)
        (when found
          (emit-byte buf #xFC) (emit-u32 buf code)
          (return-from emit-instr)))
      (ecase op
        ;; structured: (:block label bt . body)
        ((:block :loop)
         (destructuring-bind (label bt &rest body) args
           (emit-byte buf (if (eq op :block) #x02 #x03))
           (emit-blocktype buf bt)
           (let ((*labels* (cons label *labels*)))
             (dolist (i body) (emit-instr buf i)))
           (emit-byte buf #x0B)))
        ;; (:if label bt (then...) (else...))
        (:if
         (destructuring-bind (label bt then &optional else) args
           (emit-byte buf #x04)
           (emit-blocktype buf bt)
           (let ((*labels* (cons label *labels*)))
             (dolist (i then) (emit-instr buf i))
             (when else
               (emit-byte buf #x05)
               (dolist (i else) (emit-instr buf i))))
           (emit-byte buf #x0B)))
        ;; (:try_table label bt (catches...) . body)
        (:try_table
         (destructuring-bind (label bt catches &rest body) args
           (emit-byte buf #x1F)
           (emit-blocktype buf bt)
           (emit-u32 buf (length catches))
           (dolist (c catches)
             (ecase (first c)
               (:catch (emit-byte buf 0) (emit-u32 buf (tag-index m (second c)))
                (emit-u32 buf (label-depth (third c))))
               (:catch_ref (emit-byte buf 1) (emit-u32 buf (tag-index m (second c)))
                (emit-u32 buf (label-depth (third c))))
               (:catch_all (emit-byte buf 2) (emit-u32 buf (label-depth (second c))))
               (:catch_all_ref (emit-byte buf 3) (emit-u32 buf (label-depth (second c))))))
           (let ((*labels* (cons label *labels*)))
             (dolist (i body) (emit-instr buf i)))
           (emit-byte buf #x0B)))
        (:br (emit-byte buf #x0C) (emit-u32 buf (label-depth (first args))))
        (:br_if (emit-byte buf #x0D) (emit-u32 buf (label-depth (first args))))
        (:br_table
         (emit-byte buf #x0E)
         (let ((labels (butlast args)) (default (car (last args))))
           (emit-u32 buf (length labels))
           (dolist (l labels) (emit-u32 buf (label-depth l)))
           (emit-u32 buf (label-depth default))))
        (:br_on_null (emit-byte buf #xD5) (emit-u32 buf (label-depth (first args))))
        (:br_on_non_null (emit-byte buf #xD6) (emit-u32 buf (label-depth (first args))))
        (:call (emit-byte buf #x10) (emit-u32 buf (func-index m (first args))))
        (:return_call (emit-byte buf #x12) (emit-u32 buf (func-index m (first args))))
        (:call_ref (emit-byte buf #x14) (emit-u32 buf (type-index m (first args))))
        (:return_call_ref (emit-byte buf #x15) (emit-u32 buf (type-index m (first args))))
        (:throw (emit-byte buf #x08) (emit-u32 buf (tag-index m (first args))))
        (:local.get (emit-byte buf #x20) (emit-u32 buf (local-index (first args))))
        (:local.set (emit-byte buf #x21) (emit-u32 buf (local-index (first args))))
        (:local.tee (emit-byte buf #x22) (emit-u32 buf (local-index (first args))))
        (:global.get (emit-byte buf #x23) (emit-u32 buf (global-index m (first args)))
         (when (gethash (first args) (wasm-module-nonnull-globals m)) (emit-byte buf #xD4)))
        (:global.set (emit-byte buf #x24) (emit-u32 buf (global-index m (first args))))
        (:i32.const (emit-byte buf #x41) (emit-sleb buf (let ((v (first args)))
                                                           (if (>= v (expt 2 31)) (- v (expt 2 32)) v))))
        (:i64.const (emit-byte buf #x42) (emit-sleb buf (let ((v (first args)))
                                                           (if (>= v (expt 2 63)) (- v (expt 2 64)) v))))
        (:f64.const (emit-byte buf #x44)
         (let ((bits (let ((v (first args)))
                       (if (integerp v) v (float-bits (coerce v 'double-float) 52 11)))))
           (dotimes (i 8) (emit-byte buf (ldb (byte 8 (* 8 i)) bits)))))
        (:f32.const (emit-byte buf #x43)
         (let ((bits (let ((v (first args)))
                       (if (integerp v) v (float-bits (coerce v 'single-float) 23 8)))))
           (dotimes (i 4) (emit-byte buf (ldb (byte 8 (* 8 i)) bits)))))
        (:ref.null (emit-byte buf #xD0) (emit-heap-type m buf (first args)))
        (:ref.func (emit-byte buf #xD2) (declare-func-ref m (first args))
         (emit-u32 buf (func-index m (first args))))
        ;; GC with immediates
        ((:struct.new :struct.new_default)
         (emit-byte buf #xFB) (emit-u32 buf (if (eq op :struct.new) 0 1))
         (emit-u32 buf (type-index m (first args))))
        ((:struct.get :struct.get_s :struct.get_u :struct.set)
         (emit-byte buf #xFB)
         (emit-u32 buf (ecase op (:struct.get 2) (:struct.get_s 3) (:struct.get_u 4) (:struct.set 5)))
         (emit-u32 buf (type-index m (first args)))
         (emit-u32 buf (second args)))
        ((:array.new :array.new_default :array.get :array.get_s :array.get_u :array.set :array.fill)
         (emit-byte buf #xFB)
         (emit-u32 buf (ecase op (:array.new 6) (:array.new_default 7) (:array.get 11)
                         (:array.get_s 12) (:array.get_u 13) (:array.set 14) (:array.fill 16)))
         (emit-u32 buf (type-index m (first args))))
        (:array.new_fixed
         (emit-byte buf #xFB) (emit-u32 buf 8)
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (second args)))
        ((:array.new_data :array.init_data)
         (emit-byte buf #xFB) (emit-u32 buf (if (eq op :array.new_data) 9 18))
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (second args)))
        (:array.copy
         (emit-byte buf #xFB) (emit-u32 buf 17)
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (type-index m (second args))))
        (:ref.cast!
         ;; unchecked cast (the type is already known): a plain ref.cast
         (emit-byte buf #xFB) (emit-u32 buf 22) (emit-heap-type m buf (first args)))
        ((:ref.test :ref.cast)
         ;; (:ref.test ht) / (:ref.test :null ht)
         ;; checked casts: a non-null ref.cast to a rewritten type calls a
         ;; function that signals a Lisp error instead of trapping
         (let ((fn (and (eq op :ref.cast) (not (eq (first args) :null))
                        (cdr (assoc (first args) (wasm-module-cast-rewrites m))))))
           (when (and fn (not (equal fn (wasm-func-name *current-func*))))
             (emit-byte buf #x10) (emit-u32 buf (func-index m fn))
             (return-from emit-instr)))
         (emit-byte buf #xFB)
         (let ((nullable (eq (first args) :null)))
           (emit-u32 buf (if (eq op :ref.test) (if nullable 21 20) (if nullable 23 22)))
           (emit-heap-type m buf (if nullable (second args) (first args)))))
        ((:br_on_cast :br_on_cast_fail)
         ;; (:br_on_cast label from-type to-type) types as valtypes (ref ...)
         (destructuring-bind (label from to) args
           (emit-byte buf #xFB) (emit-u32 buf (if (eq op :br_on_cast) 24 25))
           (multiple-value-bind (n1 h1) (reftype-parts from)
             (multiple-value-bind (n2 h2) (reftype-parts to)
               (emit-byte buf (logior (if n1 1 0) (if n2 2 0)))
               (emit-u32 buf (label-depth label))
               (emit-heap-type m buf h1)
               (emit-heap-type m buf h2)))))))))

(defun reftype-parts (vt)
  "Returns (values nullable-p heap-type)."
  (cond ((consp vt) (if (eq (second vt) :null) (values t (third vt)) (values nil (second vt))))
        (t (let ((e (assoc vt '((:eqref . :eq) (:anyref . :any) (:funcref . :func) (:externref . :extern)
                                (:i31ref . :i31) (:structref . :struct) (:arrayref . :array)
                                (:exnref . :exn) (:nullref . :none)))))
             (unless e (error "wasm: not a reftype ~S" vt))
             (values t (cdr e))))))

(defun emit-expr (buf instrs)
  (let ((*labels* nil))
    (dolist (i instrs) (emit-instr buf i))
    (emit-byte buf #x0B)))

;;; ------------------------------------------------------------
;;; Sections
;;; ------------------------------------------------------------

(defun emit-section (out id body)
  (emit-byte out id)
  (emit-u32 out (length body))
  (emit-bytes out body))

(defun func-type-def (m f)
  (cdr (aref (wasm-module-types m) (type-index m (wasm-func-type f)))))

(defun encode-function-body (m f)
  (let* ((*current-func* f)
         (buf (make-buf))
         (*locals-map* (make-hash-table :test 'equal))
         (def (func-type-def m f))
         (params (second (if (eq (first def) :sub) (fourth def) def)))
         (nparams (length params)))
    (loop for n in (wasm-func-params-names f) for i from 0
          when n do (setf (gethash n *locals-map*) i))
    (loop for n in (wasm-func-local-names f) for i from nparams
          when n do (setf (gethash n *locals-map*) i))
    ;; locals: run-length encode
    (let ((groups nil))
      (dolist (lt (wasm-func-locals f))
        (if (and groups (equal (cdar groups) lt))
            (incf (caar groups))
            (push (cons 1 lt) groups)))
      (setf groups (nreverse groups))
      (emit-u32 buf (length groups))
      (dolist (g groups)
        (emit-u32 buf (car g))
        (emit-valtype m buf (cdr g))))
    (handler-bind ((error (lambda (c)
                            (format *error-output* "~&;; wasm encode error in function ~S: ~A~%"
                                    (wasm-func-name f) c))))
      (emit-expr buf (wasm-func-body f)))
    buf))

(defun encode-module (m)
  (let ((out (make-buf))
        (*module* m))
    (emit-bytes out #(#x00 #x61 #x73 #x6D #x01 #x00 #x00 #x00))
    ;; Encode function bodies first, so that ref.func declarations are collected.
    (let ((bodies (map 'list (lambda (f) (encode-function-body m f)) (wasm-module-funcs m))))
      ;; type section: the base types form one rec group (identical in every
      ;; module, so that types are shared across modules); types added later
      ;; are singleton groups
      (let* ((b (make-buf))
             (types (wasm-module-types m))
             (base (or (wasm-module-base-types m) (length types))))
        (emit-u32 b (+ 1 (- (length types) base)))
        (emit-byte b #x4E)
        (emit-u32 b base)
        (loop for i below base do (emit-subtype m b (cdr (aref types i))))
        (loop for i from base below (length types)
              do (emit-subtype m b (cdr (aref types i))))
        (emit-section out 1 b))
      ;; imports
      (when (plusp (+ (length (wasm-module-imports m)) (length (wasm-module-global-imports m))
                      (length (wasm-module-tag-imports m))))
        (let ((b (make-buf)))
          (emit-u32 b (+ (length (wasm-module-imports m)) (length (wasm-module-global-imports m))
                         (length (wasm-module-tag-imports m))))
          (loop for f across (wasm-module-imports m)
                do (emit-name b (wasm-func-import-module f))
                   (emit-name b (wasm-func-import-name f))
                   (emit-byte b 0)
                   (emit-u32 b (type-index m (wasm-func-type f))))
          (loop for (nil type mutable module field) across (wasm-module-global-imports m)
                do (emit-name b module) (emit-name b field)
                   (emit-byte b 3) (emit-valtype m b type) (emit-byte b (if mutable 1 0)))
          (loop for (nil type module field) across (wasm-module-tag-imports m)
                do (emit-name b module) (emit-name b field)
                   (emit-byte b 4) (emit-byte b 0) (emit-u32 b (type-index m type)))
          (emit-section out 2 b)))
      ;; function section
      (let ((b (make-buf)))
        (emit-u32 b (length (wasm-module-funcs m)))
        (loop for f across (wasm-module-funcs m)
              do (emit-u32 b (type-index m (wasm-func-type f))))
        (emit-section out 3 b))
      ;; tag section (13) comes after memory, before global
      (when (plusp (length (wasm-module-tags m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-tags m)))
          (loop for (nil type) across (wasm-module-tags m)
                do (emit-byte b 0) (emit-u32 b (type-index m type)))
          (emit-section out 13 b)))
      ;; globals
      (when (plusp (length (wasm-module-globals m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-globals m)))
          (loop for (nil type mutable init) across (wasm-module-globals m)
                do (emit-valtype m b type)
                   (emit-byte b (if mutable 1 0))
                   (emit-expr b init))
          (emit-section out 6 b)))
      ;; exports
      (when (wasm-module-exports m)
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-exports m)))
          (dolist (e (reverse (wasm-module-exports m)))
            (destructuring-bind (name kind ref) e
              (emit-name b name)
              (ecase kind
                (:func (emit-byte b 0) (emit-u32 b (func-index m ref)))
                (:global (emit-byte b 3) (emit-u32 b (global-index m ref)))
                (:tag (emit-byte b 4) (emit-u32 b (tag-index m ref))))))
          (emit-section out 7 b)))
      ;; start
      (when (wasm-module-start m)
        (let ((b (make-buf)))
          (emit-u32 b (func-index m (wasm-module-start m)))
          (emit-section out 8 b)))
      ;; element: one declarative segment with all ref.func'd functions
      (let ((refs (loop for k being the hash-keys of (wasm-module-declared-refs m) collect k)))
        (when refs
          (let ((b (make-buf)))
            (emit-u32 b 1)
            (emit-byte b 3) (emit-byte b 0)
            (emit-u32 b (length refs))
            (dolist (r refs) (emit-u32 b (func-index m r)))
            (emit-section out 9 b))))
      ;; datacount
      (when (plusp (length (wasm-module-data m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-data m)))
          (emit-section out 12 b)))
      ;; code
      (let ((b (make-buf)))
        (emit-u32 b (length bodies))
        (dolist (body bodies) (emit-vec-buf b body))
        (emit-section out 10 b))
      ;; data (passive)
      (when (plusp (length (wasm-module-data m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-data m)))
          (loop for d across (wasm-module-data m)
                do (emit-byte b 1) (emit-u32 b (length d)) (emit-bytes b d))
          (emit-section out 11 b)))
      ;; name section (function names) for nicer stack traces
      (let ((b (make-buf)) (sub (make-buf))
            (all (concatenate 'list (wasm-module-imports m) (wasm-module-funcs m))))
        (emit-name b "name")
        (emit-u32 sub (length all))
        (loop for f in all for i from 0
              do (emit-u32 sub i)
                 (emit-name sub (let ((n (or (wasm-func-display f) (wasm-func-name f))))
                                  (if (stringp n) n (let ((*package* (find-package :keyword)))
                                                      (prin1-to-string n))))))
        (emit-byte b 1)
        (emit-vec-buf b sub)
        (emit-section out 0 b)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))
