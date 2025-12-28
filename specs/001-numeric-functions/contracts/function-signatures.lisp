;;;; Function Signatures Contract
;;;;
;;;; ANSI Common Lisp function signatures for Phase 14A numeric functions.
;;;; This file serves as a contract for implementation verification.

;;; ============================================================
;;; Category 1: Basic Functions
;;; ============================================================

;; (abs number) => number
;; Returns absolute value; preserves type (integer->integer, float->float)
;; Examples: (abs -5) => 5, (abs -3.0) => 3.0

;; (signum number) => number
;; Returns -1, 0, or 1 indicating sign
;; For float: returns -1.0, 0.0, or 1.0
;; Examples: (signum -5) => -1, (signum 0) => 0, (signum 3.0) => 1.0

;; (max number &rest more-numbers) => number
;; Returns largest argument; result type follows contagion rules
;; Examples: (max 1 2 3) => 3, (max 1 2.0) => 2.0

;; (min number &rest more-numbers) => number
;; Returns smallest argument; result type follows contagion rules
;; Examples: (min 1 2 3) => 1, (min 1.0 2) => 1.0

;; (gcd &rest integers) => integer
;; Returns greatest common divisor; (gcd) => 0
;; Examples: (gcd 12 18) => 6, (gcd 12 18 24) => 6

;; (lcm &rest integers) => integer
;; Returns least common multiple; (lcm) => 1
;; Examples: (lcm 4 6) => 12, (lcm 3 4 5) => 60

;;; ============================================================
;;; Category 2: Trigonometric Functions
;;; ============================================================

;; (sin radians) => number
;; Returns sine of angle in radians
;; Examples: (sin 0) => 0.0, (sin (/ pi 2)) => 1.0

;; (cos radians) => number
;; Returns cosine of angle in radians
;; Examples: (cos 0) => 1.0, (cos pi) => -1.0

;; (tan radians) => number
;; Returns tangent of angle in radians
;; Examples: (tan 0) => 0.0, (tan (/ pi 4)) ~= 1.0

;; (asin number) => number
;; Returns arc sine in radians [-π/2, π/2]
;; For |number| > 1: returns complex
;; Examples: (asin 0) => 0.0, (asin 1) => π/2

;; (acos number) => number
;; Returns arc cosine in radians [0, π]
;; For |number| > 1: returns complex
;; Examples: (acos 1) => 0.0, (acos 0) => π/2

;; (atan y &optional x) => number
;; One arg: arc tangent in radians (-π/2, π/2)
;; Two args: angle of point (x, y) in radians (-π, π]
;; Examples: (atan 1) => π/4, (atan 1 1) => π/4, (atan -1 -1) => -3π/4

;;; ============================================================
;;; Category 3: Hyperbolic Functions
;;; ============================================================

;; (sinh number) => number
;; Returns hyperbolic sine: (e^x - e^-x) / 2
;; Examples: (sinh 0) => 0.0

;; (cosh number) => number
;; Returns hyperbolic cosine: (e^x + e^-x) / 2
;; Examples: (cosh 0) => 1.0

;; (tanh number) => number
;; Returns hyperbolic tangent: sinh/cosh
;; Examples: (tanh 0) => 0.0

;; (asinh number) => number
;; Returns inverse hyperbolic sine
;; Examples: (asinh 0) => 0.0

;; (acosh number) => number
;; Returns inverse hyperbolic cosine; number >= 1
;; Examples: (acosh 1) => 0.0

;; (atanh number) => number
;; Returns inverse hyperbolic tangent; |number| < 1
;; Examples: (atanh 0) => 0.0

;;; ============================================================
;;; Category 4: Bitwise Operations
;;; ============================================================

;; (ash integer count) => integer
;; Arithmetic shift: positive count = left, negative = right
;; Examples: (ash 1 10) => 1024, (ash 1024 -10) => 1

;; (logand &rest integers) => integer
;; Bitwise AND; (logand) => -1 (all 1s)
;; Examples: (logand #xFF00 #x0FF0) => #x0F00

;; (logior &rest integers) => integer
;; Bitwise inclusive OR; (logior) => 0
;; Examples: (logior #xFF00 #x00FF) => #xFFFF

;; (logxor &rest integers) => integer
;; Bitwise exclusive OR; (logxor) => 0
;; Examples: (logxor #xFFFF #xFF00) => #x00FF

;; (lognot integer) => integer
;; Bitwise complement (one's complement)
;; Examples: (lognot 0) => -1, (lognot -1) => 0

;; (logcount integer) => integer
;; Count of 1-bits (positive) or 0-bits (negative)
;; Examples: (logcount 13) => 3, (logcount -13) => 2

;; (integer-length integer) => integer
;; Minimum bits needed to represent (excluding sign)
;; Examples: (integer-length 0) => 0, (integer-length 7) => 3

;;; ============================================================
;;; Category 5: Mathematical Functions
;;; ============================================================

;; (exp number) => number
;; Returns e^number
;; Examples: (exp 0) => 1.0, (exp 1) ~= 2.718281828

;; (log number &optional base) => number
;; Natural log (one arg) or log base (two args)
;; Examples: (log 1) => 0.0, (log 100 10) => 2.0

;; (sqrt number) => number
;; Principal square root; negative => complex
;; Examples: (sqrt 4) => 2.0, (sqrt -1) => #C(0 1)

;; (expt base power) => number
;; Returns base^power
;; Examples: (expt 2 10) => 1024, (expt 27 1/3) => 3.0

;;; ============================================================
;;; Category 6: Complex Number Functions
;;; ============================================================

;; (complex realpart &optional imagpart) => complex
;; Creates complex number; imagpart defaults to 0
;; Examples: (complex 3 4) => #C(3 4), (complex 5) => 5

;; (realpart number) => real
;; Returns real part; for reals, returns the number
;; Examples: (realpart #C(3 4)) => 3, (realpart 5) => 5

;; (imagpart number) => real
;; Returns imaginary part; for reals, returns 0
;; Examples: (imagpart #C(3 4)) => 4, (imagpart 5) => 0

;; (conjugate number) => number
;; Complex conjugate; for reals, returns the number
;; Examples: (conjugate #C(3 4)) => #C(3 -4), (conjugate 5) => 5

;; (phase number) => float
;; Angle in radians (-π, π]
;; Examples: (phase #C(0 1)) => π/2, (phase 1) => 0.0
