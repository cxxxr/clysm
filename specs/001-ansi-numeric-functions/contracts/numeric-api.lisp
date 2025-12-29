;;;; numeric-api.lisp - API Contracts for ANSI Numeric Functions
;;;; Feature: 001-ansi-numeric-functions
;;;; Date: 2025-12-29

(in-package #:clysm/contracts/numeric)

;;; ============================================================
;;; Trigonometric Functions (FR-001 to FR-006)
;;; ============================================================

(defcontract sin (x)
  "Compute the sine of X (in radians).
   FR-001: System MUST implement sin accepting a real number and returning the sine as a float."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((sin 0) => 0.0)
  ((sin (/ pi 2)) => 1.0)
  ((sin pi) => 0.0))

(defcontract cos (x)
  "Compute the cosine of X (in radians).
   FR-002: System MUST implement cos accepting a real number and returning the cosine as a float."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((cos 0) => 1.0)
  ((cos (/ pi 2)) => 0.0)
  ((cos pi) => -1.0))

(defcontract tan (x)
  "Compute the tangent of X (in radians).
   FR-003: System MUST implement tan accepting a real number and returning the tangent as a float."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((tan 0) => 0.0)
  ((tan (/ pi 4)) => 1.0))

(defcontract asin (x)
  "Compute the arc sine of X, result in radians.
   FR-004: System MUST implement asin accepting a number in [-1, 1] and returning the arc sine in radians.
   For |x| > 1, returns complex result per ANSI CL."
  :precondition (numberp x)
  :postcondition (numberp result)
  :examples
  ((asin 0) => 0.0)
  ((asin 1) => (/ pi 2))
  ((asin 2) => #C(1.5707963... 1.316957...)))

(defcontract acos (x)
  "Compute the arc cosine of X, result in radians.
   FR-005: System MUST implement acos accepting a number in [-1, 1] and returning the arc cosine in radians.
   For |x| > 1, returns complex result per ANSI CL."
  :precondition (numberp x)
  :postcondition (numberp result)
  :examples
  ((acos 1) => 0.0)
  ((acos 0) => (/ pi 2))
  ((acos -1) => pi))

(defcontract atan (y &optional x)
  "Compute the arc tangent.
   FR-006: System MUST implement atan accepting one or two arguments and returning the arc tangent in radians.
   With one arg: atan(y).
   With two args: atan2(y, x) for correct quadrant."
  :precondition (and (realp y) (or (null x) (realp x)))
  :postcondition (floatp result)
  :examples
  ((atan 0) => 0.0)
  ((atan 1) => (/ pi 4))
  ((atan 1 1) => (/ pi 4))
  ((atan 1 -1) => (* 3 (/ pi 4))))

;;; ============================================================
;;; Hyperbolic Functions (FR-007 to FR-012)
;;; ============================================================

(defcontract sinh (x)
  "Compute the hyperbolic sine of X.
   FR-007: System MUST implement sinh accepting a real number and returning the hyperbolic sine."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((sinh 0) => 0.0))

(defcontract cosh (x)
  "Compute the hyperbolic cosine of X.
   FR-008: System MUST implement cosh accepting a real number and returning the hyperbolic cosine."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((cosh 0) => 1.0))

(defcontract tanh (x)
  "Compute the hyperbolic tangent of X.
   FR-009: System MUST implement tanh accepting a real number and returning the hyperbolic tangent."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((tanh 0) => 0.0))

(defcontract asinh (x)
  "Compute the inverse hyperbolic sine of X.
   FR-010: System MUST implement asinh accepting a real number and returning the inverse hyperbolic sine."
  :precondition (realp x)
  :postcondition (floatp result)
  :examples
  ((asinh 0) => 0.0))

(defcontract acosh (x)
  "Compute the inverse hyperbolic cosine of X.
   FR-011: System MUST implement acosh accepting a number >= 1 and returning the inverse hyperbolic cosine.
   For x < 1, returns complex result."
  :precondition (numberp x)
  :postcondition (numberp result)
  :examples
  ((acosh 1) => 0.0))

(defcontract atanh (x)
  "Compute the inverse hyperbolic tangent of X.
   FR-012: System MUST implement atanh accepting a number in (-1, 1) and returning the inverse hyperbolic tangent.
   For |x| >= 1, returns complex result."
  :precondition (numberp x)
  :postcondition (numberp result)
  :examples
  ((atanh 0) => 0.0))

;;; ============================================================
;;; Bitwise Operations (FR-013 to FR-018)
;;; ============================================================

(defcontract ash (integer count)
  "Arithmetic shift INTEGER by COUNT bits.
   FR-013: System MUST implement ash accepting an integer and shift count, returning the arithmetic-shifted result.
   Positive count: shift left. Negative count: shift right (sign-extending)."
  :precondition (and (integerp integer) (integerp count))
  :postcondition (integerp result)
  :examples
  ((ash 1 4) => 16)
  ((ash 16 -2) => 4)
  ((ash -16 -2) => -4))

(defcontract logand (&rest integers)
  "Bitwise AND of INTEGERS.
   FR-014: System MUST implement logand accepting zero or more integers, returning the bitwise AND (identity -1 for no args)."
  :precondition (every #'integerp integers)
  :postcondition (integerp result)
  :examples
  ((logand) => -1)
  ((logand #b1100 #b1010) => #b1000)
  ((logand #xFFFF #x0F0F) => #x0F0F))

(defcontract logior (&rest integers)
  "Bitwise inclusive OR of INTEGERS.
   FR-015: System MUST implement logior accepting zero or more integers, returning the bitwise inclusive OR (identity 0 for no args)."
  :precondition (every #'integerp integers)
  :postcondition (integerp result)
  :examples
  ((logior) => 0)
  ((logior #b1100 #b1010) => #b1110)
  ((logior #x0F00 #x00F0) => #x0FF0))

(defcontract logxor (&rest integers)
  "Bitwise exclusive OR of INTEGERS.
   FR-016: System MUST implement logxor accepting zero or more integers, returning the bitwise exclusive OR (identity 0 for no args)."
  :precondition (every #'integerp integers)
  :postcondition (integerp result)
  :examples
  ((logxor) => 0)
  ((logxor #b1100 #b1010) => #b0110)
  ((logxor #xFFFF #x0F0F) => #xF0F0))

(defcontract lognot (integer)
  "Bitwise complement of INTEGER.
   FR-017: System MUST implement lognot accepting an integer, returning the bitwise complement."
  :precondition (integerp integer)
  :postcondition (integerp result)
  :examples
  ((lognot 0) => -1)
  ((lognot -1) => 0)
  ((lognot #b0101) => #b...11111010))

(defcontract logcount (integer)
  "Count bits in INTEGER.
   FR-018: System MUST implement logcount accepting an integer, returning the count of 1-bits (positive) or 0-bits (negative)."
  :precondition (integerp integer)
  :postcondition (and (integerp result) (>= result 0))
  :examples
  ((logcount 0) => 0)
  ((logcount #b1011) => 3)
  ((logcount 255) => 8)
  ((logcount -1) => 0))

;;; ============================================================
;;; Complex Number Operations (FR-019 to FR-023)
;;; ============================================================

(defcontract complex (realpart &optional (imagpart 0))
  "Create a complex number.
   FR-019: System MUST implement complex accepting two real numbers and returning a complex number."
  :precondition (and (realp realpart) (realp imagpart))
  :postcondition (numberp result)
  :examples
  ((complex 3 4) => #C(3 4))
  ((complex 5) => 5)
  ((complex 0 1) => #C(0 1)))

(defcontract realpart (number)
  "Extract the real part of NUMBER.
   FR-020: System MUST implement realpart accepting a number and returning the real component."
  :precondition (numberp number)
  :postcondition (realp result)
  :examples
  ((realpart #C(3 4)) => 3)
  ((realpart 5) => 5)
  ((realpart 3.5) => 3.5))

(defcontract imagpart (number)
  "Extract the imaginary part of NUMBER.
   FR-021: System MUST implement imagpart accepting a number and returning the imaginary component (0 for reals)."
  :precondition (numberp number)
  :postcondition (realp result)
  :examples
  ((imagpart #C(3 4)) => 4)
  ((imagpart 5) => 0)
  ((imagpart 3.5) => 0))

(defcontract conjugate (number)
  "Return the complex conjugate of NUMBER.
   FR-022: System MUST implement conjugate accepting a number and returning the complex conjugate."
  :precondition (numberp number)
  :postcondition (numberp result)
  :examples
  ((conjugate #C(3 4)) => #C(3 -4))
  ((conjugate 5) => 5)
  ((conjugate #C(0 1)) => #C(0 -1)))

(defcontract phase (number)
  "Return the phase angle of NUMBER in radians.
   FR-023: System MUST implement phase accepting a number and returning the angle in radians."
  :precondition (numberp number)
  :postcondition (floatp result)
  :examples
  ((phase 1) => 0.0)
  ((phase -1) => pi)
  ((phase #C(0 1)) => (/ pi 2))
  ((phase #C(1 1)) => (/ pi 4)))

;;; ============================================================
;;; Mathematical Functions (FR-024 to FR-033)
;;; ============================================================

(defcontract exp (number)
  "Compute e raised to the power NUMBER.
   FR-024: System MUST implement exp accepting a number and returning e raised to that power."
  :precondition (numberp number)
  :postcondition (numberp result)
  :examples
  ((exp 0) => 1.0)
  ((exp 1) => 2.718281828...))

(defcontract log (number &optional base)
  "Compute the logarithm of NUMBER.
   FR-025: System MUST implement log accepting one or two arguments (number and optional base), returning the logarithm."
  :precondition (and (numberp number) (or (null base) (realp base)))
  :postcondition (numberp result)
  :examples
  ((log 1) => 0.0)
  ((log (exp 1)) => 1.0)
  ((log 100 10) => 2.0)
  ((log -1) => #C(0 pi)))

(defcontract sqrt (number)
  "Compute the principal square root of NUMBER.
   FR-026: System MUST implement sqrt accepting a number and returning the principal square root."
  :precondition (numberp number)
  :postcondition (numberp result)
  :examples
  ((sqrt 4) => 2.0)
  ((sqrt 2) => 1.4142135...)
  ((sqrt -1) => #C(0 1)))

(defcontract expt (base power)
  "Compute BASE raised to the POWER.
   FR-027: System MUST implement expt accepting a base and exponent, returning the power."
  :precondition (and (numberp base) (numberp power))
  :postcondition (numberp result)
  :examples
  ((expt 2 10) => 1024)
  ((expt 2 0.5) => 1.4142135...)
  ((expt 0 0) => 1))

(defcontract abs (number)
  "Compute the absolute value (magnitude for complex) of NUMBER.
   FR-028: System MUST implement abs accepting a number and returning the absolute value."
  :precondition (numberp number)
  :postcondition (and (realp result) (>= result 0))
  :examples
  ((abs 5) => 5)
  ((abs -5) => 5)
  ((abs -3.5) => 3.5)
  ((abs #C(3 4)) => 5.0))

(defcontract signum (number)
  "Return the sign indicator of NUMBER.
   FR-029: System MUST implement signum accepting a number and returning the sign indicator."
  :precondition (numberp number)
  :postcondition (numberp result)
  :examples
  ((signum 5) => 1)
  ((signum -5) => -1)
  ((signum 0) => 0)
  ((signum 3.5) => 1.0)
  ((signum #C(3 4)) => #C(0.6 0.8)))

(defcontract max (&rest reals)
  "Return the largest of REALS.
   FR-030: System MUST implement max accepting one or more real numbers, returning the largest."
  :precondition (and (>= (length reals) 1) (every #'realp reals))
  :postcondition (realp result)
  :examples
  ((max 1 2 3) => 3)
  ((max -5 -3 -7) => -3)
  ((max 1.5 2 0.5) => 2))

(defcontract min (&rest reals)
  "Return the smallest of REALS.
   FR-031: System MUST implement min accepting one or more real numbers, returning the smallest."
  :precondition (and (>= (length reals) 1) (every #'realp reals))
  :postcondition (realp result)
  :examples
  ((min 1 2 3) => 1)
  ((min -5 -3 -7) => -7)
  ((min 1.5 2 0.5) => 0.5))

(defcontract gcd (&rest integers)
  "Compute the greatest common divisor of INTEGERS.
   FR-032: System MUST implement gcd accepting zero or more non-negative integers, returning the GCD."
  :precondition (every #'integerp integers)
  :postcondition (and (integerp result) (>= result 0))
  :examples
  ((gcd) => 0)
  ((gcd 48 18) => 6)
  ((gcd 12 18 24) => 6)
  ((gcd 0 5) => 5))

(defcontract lcm (&rest integers)
  "Compute the least common multiple of INTEGERS.
   FR-033: System MUST implement lcm accepting zero or more non-negative integers, returning the LCM."
  :precondition (every #'integerp integers)
  :postcondition (and (integerp result) (>= result 0))
  :examples
  ((lcm) => 1)
  ((lcm 4 6) => 12)
  ((lcm 2 3 4) => 12)
  ((lcm 0 5) => 0))

;;; ============================================================
;;; FFI Integration (FR-034 to FR-035)
;;; ============================================================

(defcontract ffi-math-import (name)
  "Ensure math function is imported via FFI.
   FR-034: System MUST utilize Wasm imports (via host FFI) for standard math operations."
  :precondition (member name '(:sin :cos :tan :asin :acos :atan :atan2
                               :sinh :cosh :tanh :asinh :acosh :atanh
                               :exp :log :log10 :pow :sqrt))
  :postcondition (not (null (clysm/ffi:lookup-foreign-function
                              clysm/ffi:*ffi-environment* name))))

(defcontract type-coercion (value target-type)
  "Coerce VALUE to TARGET-TYPE for numeric operations.
   FR-035: System MUST handle type coercion appropriately (integers to floats for transcendental functions)."
  :precondition (numberp value)
  :postcondition (typep result target-type)
  :examples
  ((type-coercion 5 'float) => 5.0)
  ((type-coercion 3/4 'float) => 0.75))
