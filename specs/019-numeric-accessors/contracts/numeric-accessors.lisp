;;;; numeric-accessors.lisp - API contracts for 019-numeric-accessors
;;;; Function signatures and behavior contracts

;;; ============================================================
;;; Ratio Accessor Functions (ANSI CL)
;;; ============================================================

;;; (numerator rational) → integer
;;;
;;; Arguments:
;;;   rational - A rational number (integer or ratio)
;;;
;;; Returns:
;;;   integer - The numerator of the rational
;;;
;;; Behavior:
;;;   For integers: returns the integer itself
;;;   For ratios: returns the numerator component (after reduction)
;;;
;;; Errors:
;;;   TYPE-ERROR if argument is not a rational (float, complex, etc.)
;;;
;;; Examples:
;;;   (numerator 5) → 5
;;;   (numerator 1/3) → 1
;;;   (numerator -3/4) → -3  ; sign is on numerator
;;;   (numerator (/ 6 9)) → 2  ; reduced from 6/9 to 2/3
;;;
;;; WasmGC Implementation:
;;;   - Type dispatch using br_on_cast
;;;   - For ratio: struct.get $ratio 0
;;;   - For integer: return as-is

;;; (denominator rational) → positive-integer
;;;
;;; Arguments:
;;;   rational - A rational number (integer or ratio)
;;;
;;; Returns:
;;;   positive-integer - The denominator of the rational (always > 0)
;;;
;;; Behavior:
;;;   For integers: returns 1
;;;   For ratios: returns the denominator component (always positive)
;;;
;;; Errors:
;;;   TYPE-ERROR if argument is not a rational (float, complex, etc.)
;;;
;;; Examples:
;;;   (denominator 5) → 1
;;;   (denominator 1/3) → 3
;;;   (denominator -3/4) → 4  ; denominator always positive
;;;
;;; WasmGC Implementation:
;;;   - Type dispatch using br_on_cast
;;;   - For ratio: struct.get $ratio 1
;;;   - For integer: return (i32.const 1) ref.i31

;;; ============================================================
;;; Float Comparison Contracts (IEEE 754)
;;; ============================================================

;;; (= float1 float2) → boolean
;;;
;;; Behavior for special values:
;;;   (= nan nan) → NIL  ; NaN ≠ NaN
;;;   (= +inf +inf) → T
;;;   (= -inf -inf) → T
;;;   (= +inf -inf) → NIL
;;;   (= 0.0 -0.0) → T   ; negative zero equals positive zero
;;;
;;; WasmGC Implementation:
;;;   - Extract f64 values from $float structs
;;;   - Use f64.eq instruction (IEEE 754 compliant)

;;; (< float1 float2) → boolean
;;; (> float1 float2) → boolean
;;; (<= float1 float2) → boolean
;;; (>= float1 float2) → boolean
;;;
;;; Behavior for NaN:
;;;   All comparisons involving NaN return NIL
;;;
;;; Behavior for Infinity:
;;;   +Inf > any finite float
;;;   -Inf < any finite float
;;;
;;; WasmGC Implementation:
;;;   - Extract f64 values from $float structs
;;;   - Use f64.lt/f64.gt/f64.le/f64.ge instructions

;;; ============================================================
;;; Float Special Value Generation Contracts
;;; ============================================================

;;; (/ 1.0 0.0) → +infinity
;;; (/ -1.0 0.0) → -infinity
;;; (- +infinity +infinity) → NaN
;;; (* 0.0 +infinity) → NaN
;;; (sqrt -1.0) → NaN (when not coercing to complex)
;;;
;;; WasmGC Implementation:
;;;   - f64.div/f64.sub/f64.mul produce IEEE 754 special values
;;;   - Result boxed in $float struct
