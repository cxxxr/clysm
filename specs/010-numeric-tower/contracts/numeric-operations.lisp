;;;; Numeric Operations Contract
;;;; Specifies the signatures and behavior of numeric functions

;;; ============================================================
;;; Arithmetic Operations
;;; ============================================================

;; Addition: Variadic, returns sum of all arguments
;; (+ &rest numbers) => number
;; - No arguments: returns 0
;; - Type contagion applies
;; - Overflow from fixnum promotes to bignum

;; Subtraction: Variadic with unary negation
;; (- number &rest numbers) => number
;; - Single argument: returns negation
;; - Multiple arguments: subtracts rest from first

;; Multiplication: Variadic, returns product
;; (* &rest numbers) => number
;; - No arguments: returns 1
;; - Type contagion applies

;; Division: Variadic with unary reciprocal
;; (/ number &rest numbers) => number
;; - Single argument: returns reciprocal (ratio if integer)
;; - Multiple arguments: divides first by rest
;; - Integer / integer => ratio (not truncated)
;; - Division by zero signals DIVISION-BY-ZERO

;;; ============================================================
;;; Comparison Operations
;;; ============================================================

;; All comparisons are variadic, return T or NIL
;; Apply to real numbers only (not complex)

;; (= &rest numbers) => boolean
;; - All arguments numerically equal

;; (/= &rest numbers) => boolean
;; - All arguments pairwise distinct

;; (< &rest reals) => boolean
;; - Monotonically increasing

;; (> &rest reals) => boolean
;; - Monotonically decreasing

;; (<= &rest reals) => boolean
;; - Monotonically non-decreasing

;; (>= &rest reals) => boolean
;; - Monotonically non-increasing

;;; ============================================================
;;; Integer Division Operations
;;; ============================================================

;; All return two values: quotient and remainder

;; (floor number &optional divisor) => integer, real
;; - Quotient truncated toward negative infinity

;; (ceiling number &optional divisor) => integer, real
;; - Quotient truncated toward positive infinity

;; (truncate number &optional divisor) => integer, real
;; - Quotient truncated toward zero

;; (round number &optional divisor) => integer, real
;; - Quotient rounded to nearest integer (banker's rounding)

;; (mod number divisor) => real
;; - Remainder from floor: (- number (* (floor number divisor) divisor))

;; (rem number divisor) => real
;; - Remainder from truncate: (- number (* (truncate number divisor) divisor))

;;; ============================================================
;;; Mathematical Functions
;;; ============================================================

;; (sqrt number) => number
;; - Positive real: returns float
;; - Negative real: returns complex
;; - Complex: returns complex

;; (expt base power) => number
;; - Integer base, non-negative integer power: returns integer
;; - Otherwise: returns float or complex

;; (abs number) => number
;; - Real: returns non-negative of same type
;; - Complex: returns float (magnitude)

;; (gcd &rest integers) => integer
;; - Returns greatest common divisor
;; - No arguments: returns 0
;; - Always non-negative

;; (lcm &rest integers) => integer
;; - Returns least common multiple
;; - No arguments: returns 1
;; - Always non-negative

;;; ============================================================
;;; Type Predicates
;;; ============================================================

;; All return T or NIL

;; (numberp object) => boolean
;; - True for fixnum, bignum, ratio, float, complex

;; (integerp object) => boolean
;; - True for fixnum, bignum

;; (rationalp object) => boolean
;; - True for fixnum, bignum, ratio

;; (realp object) => boolean
;; - True for fixnum, bignum, ratio, float
;; - Also true for complex with zero imaginary

;; (floatp object) => boolean
;; - True for float

;; (complexp object) => boolean
;; - True for complex

;; (zerop number) => boolean
;; - True if number = 0

;; (plusp real) => boolean
;; - True if real > 0

;; (minusp real) => boolean
;; - True if real < 0

;; (evenp integer) => boolean
;; - True if integer is even

;; (oddp integer) => boolean
;; - True if integer is odd

;;; ============================================================
;;; Type Conversion Functions
;;; ============================================================

;; (float number &optional prototype) => float
;; - Converts to float

;; (rational real) => rational
;; - Converts float to exact rational

;; (numerator rational) => integer
;; - Returns numerator of rational

;; (denominator rational) => integer
;; - Returns denominator of rational (always positive)

;; (realpart number) => real
;; - Returns real part

;; (imagpart number) => real
;; - Returns imaginary part (0 for reals)

;; (complex realpart &optional imagpart) => number
;; - Creates complex number (simplifies if possible)

;;; ============================================================
;;; Implementation Notes
;;; ============================================================

;; 1. All numeric operations must handle mixed types via contagion
;; 2. Fixnum overflow must automatically promote to bignum
;; 3. Ratios must be automatically reduced to lowest terms
;; 4. Complex with exact zero imaginary must simplify to real
;; 5. All operations must generate valid WasmGC code
