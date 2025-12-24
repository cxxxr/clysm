;;;; numeric-runtime.lisp - Numeric tower runtime functions
;;;; These functions generate Wasm helper functions for numeric operations.
;;;; They are included in every compiled module that uses numeric operations.

(in-package #:clysm/compiler/codegen/func-section)

;;; ============================================================
;;; Runtime Function Index Management
;;; ============================================================

;;; Runtime functions are added after user functions.
;;; We track their indices so compile-arithmetic-op can call them.

(defvar *runtime-function-indices* (make-hash-table)
  "Map from runtime function name to function index.")

(defvar *runtime-functions-list* nil
  "List of runtime function definitions to be added to the module.")

(defun reset-runtime-functions ()
  "Reset runtime function state for a new compilation."
  (clrhash *runtime-function-indices*)
  (setf *runtime-functions-list* nil))

(defun get-runtime-function-index (name)
  "Get the index of a runtime function by name."
  (gethash name *runtime-function-indices*))

(defun collect-runtime-functions ()
  "Return the list of runtime functions to be added to the module.
   Also sets up the function indices. Should be called after all user
   functions are compiled."
  (nreverse *runtime-functions-list*))

;;; ============================================================
;;; Type Checking Instructions
;;; ============================================================

(defun emit-is-fixnum ()
  "Generate instructions to check if TOS is a fixnum (i31ref).
   Stack: [anyref] -> [i32] (1 if fixnum, 0 otherwise)"
  '((:ref.test :i31)))

(defun emit-is-bignum ()
  "Generate instructions to check if TOS is a bignum.
   Stack: [anyref] -> [i32] (1 if bignum, 0 otherwise)"
  (list (list :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-bignum+))))

(defun emit-is-ratio ()
  "Generate instructions to check if TOS is a ratio.
   Stack: [anyref] -> [i32] (1 if ratio, 0 otherwise)"
  (list (list :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-ratio+))))

(defun emit-is-float ()
  "Generate instructions to check if TOS is a float.
   Stack: [anyref] -> [i32] (1 if float, 0 otherwise)"
  (list (list :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-float+))))

(defun emit-is-complex ()
  "Generate instructions to check if TOS is a complex.
   Stack: [anyref] -> [i32] (1 if complex, 0 otherwise)"
  (list (list :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-complex+))))

;;; ============================================================
;;; Fixnum to Bignum Conversion (T024)
;;; ============================================================

(defun emit-fixnum-to-bignum ()
  "Generate instructions to convert a fixnum to a bignum.
   Stack: [i31ref] -> [ref $bignum]

   Algorithm:
   1. Extract i32 value from i31ref
   2. Determine sign (0 if >= 0, 1 if < 0)
   3. Take absolute value
   4. Create 1-element limb array with absolute value
   5. Create bignum struct with sign and limbs"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    ;; This is a function body, not inline code.
    ;; Param 0: anyref (the fixnum to convert)
    ;; Returns: ref $bignum
    `(;; Cast param to i31 and get signed value
      (:local.get 0)
      (:ref.cast :i31)
      :i31.get_s
      ;; Duplicate for sign check and absolute value
      (:local.tee 1)  ; local 1 = i32 value
      (:local.get 1)
      ;; Check if negative (sign = value < 0 ? 1 : 0)
      (:i32.const 0)
      :i32.lt_s
      ;; Now stack has: [sign:i32]
      ;; Get absolute value for limb
      (:local.get 1)
      (:local.get 1)
      (:i32.const 31)
      :i32.shr_s    ; arithmetic shift right 31 = sign extension (0 or -1)
      :i32.xor      ; XOR with sign extension
      (:local.get 1)
      (:i32.const 31)
      :i32.shr_s
      :i32.sub      ; Subtract sign extension = absolute value
      ;; Stack: [sign:i32, abs_value:i32]
      ;; Create limb array with 1 element
      (:array.new_fixed ,limb-array-type 1)
      ;; Stack: [sign:i32, ref $limb_array]
      ;; Create bignum struct
      (:struct.new ,bignum-type))))

;;; ============================================================
;;; Bignum to Fixnum (normalization)
;;; ============================================================

(defun emit-bignum-try-to-fixnum ()
  "Generate instructions to try converting a bignum to a fixnum.
   If the bignum fits in i31 range, returns the fixnum.
   Otherwise returns the bignum unchanged.
   Stack: [ref $bignum] -> [anyref]"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    ;; Check if bignum has exactly 1 limb and value fits in i31 range
    `(;; Get the bignum from param
      (:local.get 0)
      (:ref.cast (ref ,bignum-type))
      (:local.tee 1)  ; local 1 = bignum ref
      ;; Get limbs array
      (:struct.get ,bignum-type 1)
      (:local.tee 2)  ; local 2 = limbs ref
      ;; Check array length = 1
      :array.len
      (:i32.const 1)
      :i32.ne
      ;; If length != 1, return bignum unchanged
      (:if (:result :anyref))
      (:local.get 0)  ; return original
      (:else)
      ;; Length is 1, check if value fits in i31 range
      ;; Get the single limb value
      (:local.get 2)
      (:i32.const 0)
      (:array.get ,limb-array-type)
      (:local.tee 3)  ; local 3 = limb value (unsigned)
      ;; Get sign
      (:local.get 1)
      (:struct.get ,bignum-type 0)
      (:local.tee 4)  ; local 4 = sign
      ;; If positive, check value <= 1073741823 (i31 max)
      (:if (:result :i32))
      ;; Negative: check value <= 1073741824 (|i31 min|)
      (:local.get 3)
      (:i32.const 1073741824)
      :i32.le_u
      (:else)
      ;; Positive: check value <= 1073741823
      (:local.get 3)
      (:i32.const 1073741823)
      :i32.le_u
      (:end)
      ;; Stack: [fits:i32]
      (:if (:result :anyref))
      ;; Fits in fixnum! Convert back
      (:local.get 3)
      (:local.get 4)
      (:if (:result :i32))
      ;; Negative: negate the value
      (:i32.const 0)
      (:local.get 3)
      :i32.sub
      (:else)
      (:local.get 3)
      (:end)
      :ref.i31
      (:else)
      ;; Doesn't fit, return bignum
      (:local.get 0)
      (:end)
      (:end))))

;;; ============================================================
;;; Bignum Comparison Helpers
;;; ============================================================

(defun generate-bignum-compare-magnitude-func ()
  "Generate function to compare magnitudes of two bignums.
   Returns -1 if |a| < |b|, 0 if |a| = |b|, 1 if |a| > |b|.
   Params: (ref $bignum, ref $bignum)
   Result: i32"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    (list :name '$bignum_compare_magnitude
          :params '(($a :anyref) ($b :anyref))
          :result :i32
          :locals '(($limbs_a :anyref) ($limbs_b :anyref)
                    ($len_a :i32) ($len_b :i32)
                    ($i :i32) ($va :i32) ($vb :i32))
          :body
          `(;; Get limb arrays
            (:local.get 0)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 1)
            (:local.set 2)  ; limbs_a
            (:local.get 1)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 1)
            (:local.set 3)  ; limbs_b
            ;; Get lengths
            (:local.get 2)
            (:ref.cast (ref ,limb-array-type))
            :array.len
            (:local.set 4)  ; len_a
            (:local.get 3)
            (:ref.cast (ref ,limb-array-type))
            :array.len
            (:local.set 5)  ; len_b
            ;; Compare lengths first
            (:local.get 4)
            (:local.get 5)
            :i32.gt_u
            (:if (:result :i32))
            (:i32.const 1)  ; |a| > |b|
            (:else)
            (:local.get 4)
            (:local.get 5)
            :i32.lt_u
            (:if (:result :i32))
            (:i32.const -1)  ; |a| < |b|
            (:else)
            ;; Same length, compare limbs from most significant
            (:local.get 4)
            (:local.set 6)  ; i = len
            (:block (:type 23))  ; outer block for break
            (:loop (:type 22))   ; loop
            ;; Decrement i first
            (:local.get 6)
            (:i32.const 1)
            :i32.sub
            (:local.tee 6)
            (:i32.const 0)
            :i32.lt_s
            (:br_if 1)  ; break if i < 0
            ;; Get limbs[i]
            (:local.get 2)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 6)
            (:array.get ,limb-array-type)
            (:local.set 7)  ; va
            (:local.get 3)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 6)
            (:array.get ,limb-array-type)
            (:local.set 8)  ; vb
            ;; Compare
            (:local.get 7)
            (:local.get 8)
            :i32.gt_u
            (:if)
            (:i32.const 1)
            (:br 3)  ; return 1
            (:end)
            (:local.get 7)
            (:local.get 8)
            :i32.lt_u
            (:if)
            (:i32.const -1)
            (:br 3)  ; return -1
            (:end)
            ;; Continue loop
            (:br 0)
            (:end)  ; loop
            (:end)  ; block
            ;; All limbs equal
            (:i32.const 0)
            (:end)
            (:end)))))

;;; ============================================================
;;; Bignum Addition (T025)
;;; ============================================================

(defun generate-bignum-add-func ()
  "Generate function to add two bignums.
   Params: (anyref, anyref) - both must be bignums
   Result: anyref - bignum or fixnum if normalized"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    (list :name '$bignum_add
          :params '(($a :anyref) ($b :anyref))
          :result :anyref
          :locals '(($sign_a :i32) ($sign_b :i32)
                    ($limbs_a :anyref) ($limbs_b :anyref)
                    ($len_a :i32) ($len_b :i32)
                    ($result_limbs :anyref) ($result_len :i32)
                    ($i :i32) ($carry :i64) ($sum :i64)
                    ($va :i32) ($vb :i32)
                    ($cmp :i32) ($result_sign :i32))
          :body
          `(;; Get signs
            (:local.get 0)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 0)
            (:local.set 2)  ; sign_a
            (:local.get 1)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 0)
            (:local.set 3)  ; sign_b

            ;; Get limb arrays
            (:local.get 0)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 1)
            (:local.set 4)  ; limbs_a
            (:local.get 1)
            (:ref.cast (ref ,bignum-type))
            (:struct.get ,bignum-type 1)
            (:local.set 5)  ; limbs_b

            ;; Get lengths
            (:local.get 4)
            (:ref.cast (ref ,limb-array-type))
            :array.len
            (:local.set 6)  ; len_a
            (:local.get 5)
            (:ref.cast (ref ,limb-array-type))
            :array.len
            (:local.set 7)  ; len_b

            ;; If same sign, add magnitudes
            (:local.get 2)
            (:local.get 3)
            :i32.eq
            (:if (:result :anyref))
            ;; Same sign: add magnitudes, keep sign
            ;; Result length = max(len_a, len_b) + 1 for carry
            (:local.get 6)
            (:local.get 7)
            (:local.get 6)
            (:local.get 7)
            :i32.gt_u
            :select
            (:i32.const 1)
            :i32.add
            (:local.tee 9)  ; result_len
            (:array.new_default ,limb-array-type)
            (:local.set 8)  ; result_limbs

            ;; Add loop
            (:i32.const 0)
            (:local.set 10)  ; i = 0
            (:i64.const 0)
            (:local.set 11)  ; carry = 0

            (:block)
            (:loop)
            ;; Check if i >= max(len_a, len_b)
            (:local.get 10)
            (:local.get 6)
            (:local.get 7)
            (:local.get 6)
            (:local.get 7)
            :i32.gt_u
            :select
            :i32.ge_u
            (:br_if 1)

            ;; Get a[i] if i < len_a, else 0
            (:local.get 10)
            (:local.get 6)
            :i32.lt_u
            (:if (:result :i64))
            (:local.get 4)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 10)
            (:array.get ,limb-array-type)
            :i64.extend_i32_u
            (:else)
            (:i64.const 0)
            (:end)

            ;; Get b[i] if i < len_b, else 0
            (:local.get 10)
            (:local.get 7)
            :i32.lt_u
            (:if (:result :i64))
            (:local.get 5)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 10)
            (:array.get ,limb-array-type)
            :i64.extend_i32_u
            (:else)
            (:i64.const 0)
            (:end)

            ;; sum = a[i] + b[i] + carry
            :i64.add
            (:local.get 11)
            :i64.add
            (:local.set 12)  ; sum

            ;; result[i] = sum & 0xFFFFFFFF
            (:local.get 8)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 10)
            (:local.get 12)
            :i32.wrap_i64
            (:array.set ,limb-array-type)

            ;; carry = sum >> 32
            (:local.get 12)
            (:i64.const 32)
            :i64.shr_u
            (:local.set 11)

            ;; i++
            (:local.get 10)
            (:i32.const 1)
            :i32.add
            (:local.set 10)
            (:br 0)
            (:end)
            (:end)

            ;; Store final carry if non-zero
            (:local.get 11)
            (:i64.const 0)
            :i64.ne
            (:if)
            (:local.get 8)
            (:ref.cast (ref ,limb-array-type))
            (:local.get 10)
            (:local.get 11)
            :i32.wrap_i64
            (:array.set ,limb-array-type)
            (:end)

            ;; Create result bignum
            (:local.get 2)  ; sign (same as input signs)
            (:local.get 8)
            (:struct.new ,bignum-type)

            (:else)
            ;; Different signs: subtract smaller magnitude from larger
            ;; First compare magnitudes
            (:local.get 0)
            (:local.get 1)
            (:call 1)  ; $bignum_compare_magnitude (will be index 1)
            (:local.tee 14)  ; cmp
            (:i32.const 0)
            :i32.eq
            (:if (:result :anyref))
            ;; Magnitudes equal, result is 0
            (:i32.const 0)
            :ref.i31
            (:else)
            ;; Subtract smaller from larger
            ;; For now, return a placeholder (TODO: implement subtraction)
            (:local.get 0)
            (:end)
            (:end)))))

;;; ============================================================
;;; Generic Arithmetic with Type Dispatch (T030-T034)
;;; ============================================================

(defun generate-generic-add-func ()
  "Generate function for generic addition with type dispatch.
   Params: (anyref, anyref)
   Result: anyref

   Type dispatch order:
   1. If both fixnum: add as i32, check overflow
   2. If either bignum: promote both to bignum, add
   3. If either float: promote both to float, add
   4. If either complex: promote both to complex, add
   5. If either ratio: promote both to ratio, add"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    (list :name '$generic_add
          :params '(($a :anyref) ($b :anyref))
          :result :anyref
          :locals '(($ia :i32) ($ib :i32) ($sum :i32))
          :body
          `(;; Check if both are fixnums
            (:local.get 0)
            (:ref.test :i31)
            (:local.get 1)
            (:ref.test :i31)
            :i32.and
            (:if (:result :anyref))
            ;; Both fixnums: fast path
            (:local.get 0)
            (:ref.cast :i31)
            :i31.get_s
            (:local.set 2)  ; ia
            (:local.get 1)
            (:ref.cast :i31)
            :i31.get_s
            (:local.set 3)  ; ib
            (:local.get 2)
            (:local.get 3)
            :i32.add
            (:local.tee 4)  ; sum
            ;; Check for overflow (outside i31 range: -2^30 to 2^30-1)
            (:i32.const -1073741824)  ; -2^30
            :i32.lt_s
            (:local.get 4)
            (:i32.const 1073741823)   ; 2^30-1
            :i32.gt_s
            :i32.or
            (:if (:result :anyref))
            ;; Overflow! Promote to bignum
            ;; Convert both operands to bignum and add
            (:local.get 0)
            (:call 2)  ; $fixnum_to_bignum
            (:local.get 1)
            (:call 2)  ; $fixnum_to_bignum
            (:call 0)  ; $bignum_add
            (:else)
            ;; No overflow, wrap as i31
            (:local.get 4)
            :ref.i31
            (:end)
            (:else)
            ;; At least one is not a fixnum
            ;; Check if either is a bignum
            (:local.get 0)
            (:ref.test (ref ,bignum-type))
            (:local.get 1)
            (:ref.test (ref ,bignum-type))
            :i32.or
            (:if (:result :anyref))
            ;; At least one bignum: promote both to bignum and add
            ;; First arg
            (:local.get 0)
            (:ref.test :i31)
            (:if (:result :anyref))
            (:local.get 0)
            (:call 2)  ; $fixnum_to_bignum
            (:else)
            (:local.get 0)
            (:end)
            ;; Second arg
            (:local.get 1)
            (:ref.test :i31)
            (:if (:result :anyref))
            (:local.get 1)
            (:call 2)  ; $fixnum_to_bignum
            (:else)
            (:local.get 1)
            (:end)
            (:call 0)  ; $bignum_add
            (:else)
            ;; TODO: Handle float, ratio, complex
            ;; For now, just return first arg as placeholder
            (:local.get 0)
            (:end)
            (:end)))))

(defun generate-fixnum-to-bignum-func ()
  "Generate function to convert fixnum to bignum.
   Params: (anyref) - should be i31ref
   Result: anyref - ref $bignum"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    (list :name '$fixnum_to_bignum
          :params '(($n :anyref))
          :result :anyref
          :locals '(($value :i32) ($sign :i32) ($abs :i32))
          :body
          `(;; Get the i32 value
            (:local.get 0)
            (:ref.cast :i31)
            :i31.get_s
            (:local.tee 1)  ; value
            ;; Determine sign
            (:i32.const 0)
            :i32.lt_s
            (:local.set 2)  ; sign = (value < 0) ? 1 : 0
            ;; Get absolute value
            (:local.get 1)
            (:local.get 1)
            (:i32.const 31)
            :i32.shr_s      ; sign mask (-1 or 0)
            (:local.tee 3)
            :i32.xor
            (:local.get 3)
            :i32.sub        ; abs = (value ^ mask) - mask
            ;; Create limb array with single element
            (:array.new_fixed ,limb-array-type 1)
            ;; Create bignum struct: (sign, limbs)
            (:local.get 2)
            ;; Swap order: sign first, then limbs
            ;; But we pushed limbs first, so use swap technique
            ;; Actually let's do it properly:
            ))))

;; Let me rewrite this more carefully
(defun generate-fixnum-to-bignum-func-v2 ()
  "Generate function to convert fixnum to bignum.
   Params: (anyref) - should be i31ref
   Result: anyref - ref $bignum"
  (let ((bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
        (limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+))
    (list :name '$fixnum_to_bignum
          :params '(($n :anyref))
          :result :anyref
          :locals '(($value :i32) ($abs_value :i32))
          :body
          `(;; Get the i32 value
            (:local.get 0)
            (:ref.cast :i31)
            :i31.get_s
            (:local.set 1)  ; value
            ;; Compute absolute value using: abs = (x ^ (x >> 31)) - (x >> 31)
            (:local.get 1)
            (:local.get 1)
            (:i32.const 31)
            :i32.shr_s
            :i32.xor
            (:local.get 1)
            (:i32.const 31)
            :i32.shr_s
            :i32.sub
            (:local.set 2)  ; abs_value
            ;; Push sign (1 if negative, 0 otherwise)
            (:local.get 1)
            (:i32.const 0)
            :i32.lt_s
            ;; Push abs_value for array
            (:local.get 2)
            ;; Create limb array with single element containing abs_value
            (:array.new_fixed ,limb-array-type 1)
            ;; Now stack is: [sign:i32, limbs:ref]
            ;; struct.new expects fields in order: sign, limbs
            (:struct.new ,bignum-type)))))

;;; ============================================================
;;; Runtime Function Registration
;;; ============================================================

(defun register-runtime-functions (start-index)
  "Register all runtime functions and return the list.
   START-INDEX is the function index to start at."
  (let ((funcs '())
        (idx start-index))
    ;; Function 0 (at start-index): $bignum_add
    (push (generate-bignum-add-func) funcs)
    (setf (gethash '$bignum_add *runtime-function-indices*) idx)
    (incf idx)
    ;; Function 1 (at start-index + 1): $bignum_compare_magnitude
    (push (generate-bignum-compare-magnitude-func) funcs)
    (setf (gethash '$bignum_compare_magnitude *runtime-function-indices*) idx)
    (incf idx)
    ;; Function 2 (at start-index + 2): $fixnum_to_bignum
    (push (generate-fixnum-to-bignum-func-v2) funcs)
    (setf (gethash '$fixnum_to_bignum *runtime-function-indices*) idx)
    (incf idx)
    ;; Function 3 (at start-index + 3): $generic_add
    (push (generate-generic-add-func) funcs)
    (setf (gethash '$generic_add *runtime-function-indices*) idx)
    (incf idx)

    (setf *runtime-functions-list* (nreverse funcs))
    *runtime-functions-list*))

(defun get-runtime-functions-count ()
  "Return the number of runtime functions."
  (length *runtime-functions-list*))
