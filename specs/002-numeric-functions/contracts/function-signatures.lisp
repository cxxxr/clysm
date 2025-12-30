;;; Function Signatures Contract
;;; Phase 14A - Basic Arithmetic Function Extension
;;; Date: 2025-12-30

;;; This file defines the function signatures, type contracts, and
;;; expected behaviors for all functions implemented in this feature.

;;; ============================================================
;;; TRIGONOMETRIC FUNCTIONS
;;; HyperSpec: resources/HyperSpec/Body/f_sin_c.htm
;;;            resources/HyperSpec/Body/f_asin_.htm
;;; ============================================================

;;; sin: number -> number
;;; Returns the sine of the argument (in radians)
(defcontract sin
  :lambda-list (radians)
  :argument-types ((radians number))
  :return-type number
  :preconditions ()
  :postconditions ((numberp result))
  :examples
  ((sin 0) => 0.0)
  ((sin (/ pi 2)) => 1.0 :tolerance 1e-10)
  ((sin pi) => 0.0 :tolerance 1e-10))

;;; cos: number -> number
;;; Returns the cosine of the argument (in radians)
(defcontract cos
  :lambda-list (radians)
  :argument-types ((radians number))
  :return-type number
  :preconditions ()
  :postconditions ((numberp result))
  :examples
  ((cos 0) => 1.0)
  ((cos (/ pi 2)) => 0.0 :tolerance 1e-10)
  ((cos pi) => -1.0 :tolerance 1e-10))

;;; tan: number -> number
;;; Returns the tangent of the argument (in radians)
(defcontract tan
  :lambda-list (radians)
  :argument-types ((radians number))
  :return-type number
  :preconditions ()
  :postconditions ((numberp result))
  :examples
  ((tan 0) => 0.0)
  ((tan (/ pi 4)) => 1.0 :tolerance 1e-10))

;;; asin: number -> number
;;; Returns the arc sine of the argument
(defcontract asin
  :lambda-list (number)
  :argument-types ((number (real -1 1)))
  :return-type number
  :preconditions ((<= -1 number 1))
  :postconditions ((numberp result))
  :examples
  ((asin 0) => 0.0)
  ((asin 1) => (/ pi 2) :tolerance 1e-10)
  ((asin -1) => (- (/ pi 2)) :tolerance 1e-10))

;;; acos: number -> number
;;; Returns the arc cosine of the argument
(defcontract acos
  :lambda-list (number)
  :argument-types ((number (real -1 1)))
  :return-type number
  :preconditions ((<= -1 number 1))
  :postconditions ((numberp result))
  :examples
  ((acos 1) => 0.0)
  ((acos 0) => (/ pi 2) :tolerance 1e-10)
  ((acos -1) => pi :tolerance 1e-10))

;;; atan: number &optional number -> number
;;; Returns the arc tangent; two-argument form computes atan2
(defcontract atan
  :lambda-list (y &optional x)
  :argument-types ((y number) (x (or null number)))
  :return-type number
  :preconditions ()
  :postconditions ((numberp result))
  :examples
  ((atan 0) => 0.0)
  ((atan 1) => (/ pi 4) :tolerance 1e-10)
  ((atan 1 1) => (/ pi 4) :tolerance 1e-10)
  ((atan 1 -1) => (* 3 (/ pi 4)) :tolerance 1e-10))

;;; ============================================================
;;; HYPERBOLIC FUNCTIONS
;;; HyperSpec: resources/HyperSpec/Body/f_sinh_.htm
;;; ============================================================

;;; sinh: number -> number
;;; Returns the hyperbolic sine
(defcontract sinh
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :examples
  ((sinh 0) => 0.0)
  ((sinh 1) => 1.1752011936438014 :tolerance 1e-10))

;;; cosh: number -> number
;;; Returns the hyperbolic cosine
(defcontract cosh
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :examples
  ((cosh 0) => 1.0)
  ((cosh 1) => 1.5430806348152437 :tolerance 1e-10))

;;; tanh: number -> number
;;; Returns the hyperbolic tangent
(defcontract tanh
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :examples
  ((tanh 0) => 0.0)
  ((tanh 1) => 0.7615941559557649 :tolerance 1e-10))

;;; asinh: number -> number
;;; Returns the inverse hyperbolic sine
(defcontract asinh
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :examples
  ((asinh 0) => 0.0)
  ((asinh 1) => 0.881373587019543 :tolerance 1e-10))

;;; acosh: number -> number
;;; Returns the inverse hyperbolic cosine
(defcontract acosh
  :lambda-list (number)
  :argument-types ((number (real 1 *)))
  :return-type number
  :preconditions ((>= number 1))
  :examples
  ((acosh 1) => 0.0)
  ((acosh 2) => 1.3169578969248166 :tolerance 1e-10))

;;; atanh: number -> number
;;; Returns the inverse hyperbolic tangent
(defcontract atanh
  :lambda-list (number)
  :argument-types ((number (real -1 1)))
  :return-type number
  :preconditions ((< -1 number 1))
  :examples
  ((atanh 0) => 0.0)
  ((atanh 0.5) => 0.5493061443340548 :tolerance 1e-10))

;;; ============================================================
;;; BIT OPERATION FUNCTIONS
;;; HyperSpec: resources/HyperSpec/Body/f_ash.htm
;;;            resources/HyperSpec/Body/f_logand.htm
;;;            resources/HyperSpec/Body/f_logcou.htm
;;; ============================================================

;;; ash: integer integer -> integer
;;; Arithmetic shift; positive count = left, negative = right
(defcontract ash
  :lambda-list (integer count)
  :argument-types ((integer integer) (count integer))
  :return-type integer
  :postconditions ((integerp result))
  :examples
  ((ash 1 4) => 16)
  ((ash 16 -4) => 1)
  ((ash 1 10) => 1024)
  ((ash 1024 -10) => 1)
  ((ash -1 10) => -1024)
  ((ash -1024 -10) => -1))

;;; logand: &rest integers -> integer
;;; Bitwise AND; identity is -1
(defcontract logand
  :lambda-list (&rest integers)
  :argument-types ((integers (list-of integer)))
  :return-type integer
  :postconditions ((integerp result))
  :examples
  ((logand) => -1)
  ((logand 15) => 15)
  ((logand #xFF #x0F) => #x0F)
  ((logand #b1111 #b1010) => #b1010))

;;; logior: &rest integers -> integer
;;; Bitwise OR; identity is 0
(defcontract logior
  :lambda-list (&rest integers)
  :argument-types ((integers (list-of integer)))
  :return-type integer
  :postconditions ((integerp result))
  :examples
  ((logior) => 0)
  ((logior 15) => 15)
  ((logior #xF0 #x0F) => #xFF)
  ((logior #b1100 #b0011) => #b1111))

;;; logxor: &rest integers -> integer
;;; Bitwise XOR; identity is 0
(defcontract logxor
  :lambda-list (&rest integers)
  :argument-types ((integers (list-of integer)))
  :return-type integer
  :postconditions ((integerp result))
  :examples
  ((logxor) => 0)
  ((logxor 15) => 15)
  ((logxor #xFF #x0F) => #xF0)
  ((logxor #b1111 #b1010) => #b0101))

;;; lognot: integer -> integer
;;; Bitwise NOT (one's complement)
(defcontract lognot
  :lambda-list (integer)
  :argument-types ((integer integer))
  :return-type integer
  :postconditions ((integerp result)
                   (= (lognot result) integer))
  :examples
  ((lognot 0) => -1)
  ((lognot -1) => 0)
  ((lognot 1) => -2)
  ((lognot #xFF) => #x-100))

;;; logcount: integer -> (integer 0 *)
;;; Count of 1 bits (positive) or 0 bits (negative)
(defcontract logcount
  :lambda-list (integer)
  :argument-types ((integer integer))
  :return-type (integer 0 *)
  :postconditions ((>= result 0))
  :examples
  ((logcount 0) => 0)
  ((logcount 1) => 1)
  ((logcount #xFF) => 8)
  ((logcount #b101010) => 3)
  ((logcount -1) => 0)     ; All 1s in two's complement
  ((logcount -2) => 1))    ; One 0 bit in two's complement

;;; ============================================================
;;; MATHEMATICAL FUNCTIONS
;;; HyperSpec: resources/HyperSpec/Body/f_exp_e.htm
;;;            resources/HyperSpec/Body/f_log.htm
;;;            resources/HyperSpec/Body/f_sqrt_.htm
;;;            resources/HyperSpec/Body/f_abs.htm
;;;            resources/HyperSpec/Body/f_signum.htm
;;; ============================================================

;;; exp: number -> number
;;; Returns e raised to the power of the argument
(defcontract exp
  :lambda-list (power)
  :argument-types ((power number))
  :return-type number
  :postconditions ((numberp result))
  :examples
  ((exp 0) => 1.0)
  ((exp 1) => 2.718281828459045 :tolerance 1e-10)
  ((exp -1) => 0.36787944117144233 :tolerance 1e-10))

;;; log: number &optional number -> number
;;; Natural log (1 arg) or log base (2 args)
(defcontract log
  :lambda-list (number &optional base)
  :argument-types ((number (number (0 *)))
                   (base (or null (number (0 *)))))
  :return-type number
  :preconditions ((> number 0)
                  (or (null base) (> base 0)))
  :examples
  ((log 1) => 0.0)
  ((log (exp 1)) => 1.0 :tolerance 1e-10)
  ((log 100 10) => 2.0 :tolerance 1e-10)
  ((log 8 2) => 3.0 :tolerance 1e-10))

;;; sqrt: number -> number
;;; Returns the square root
(defcontract sqrt
  :lambda-list (number)
  :argument-types ((number (real 0 *)))
  :return-type number
  :preconditions ((>= number 0))
  :postconditions ((numberp result))
  :examples
  ((sqrt 0) => 0.0)
  ((sqrt 1) => 1.0)
  ((sqrt 4) => 2.0)
  ((sqrt 2) => 1.4142135623730951 :tolerance 1e-10))

;;; expt: number number -> number
;;; Returns base raised to power
(defcontract expt
  :lambda-list (base power)
  :argument-types ((base number) (power number))
  :return-type number
  :postconditions ((numberp result))
  :examples
  ((expt 2 0) => 1)
  ((expt 2 10) => 1024)
  ((expt 10 3) => 1000)
  ((expt 2 -1) => 0.5)
  ((expt 0 0) => 1))  ; ANSI CL convention

;;; abs: number -> number
;;; Returns absolute value (type preserved)
(defcontract abs
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :postconditions ((>= result 0)
                   (typep result (type-of number)))
  :examples
  ((abs 0) => 0)
  ((abs 5) => 5)
  ((abs -5) => 5)
  ((abs -3.14) => 3.14)
  ((abs 3/4) => 3/4)
  ((abs -3/4) => 3/4))

;;; signum: number -> number
;;; Returns -1, 0, or 1 of same type as argument
(defcontract signum
  :lambda-list (number)
  :argument-types ((number number))
  :return-type number
  :postconditions ((member result '(-1 0 1 -1.0 0.0 1.0)))
  :examples
  ((signum 0) => 0)
  ((signum 42) => 1)
  ((signum -42) => -1)
  ((signum 3.14) => 1.0)
  ((signum -3.14) => -1.0)
  ((signum 0.0) => 0.0))

;;; ============================================================
;;; TYPE CONVERSION FUNCTIONS
;;; HyperSpec: resources/HyperSpec/Body/f_float.htm
;;;            resources/HyperSpec/Body/f_ration.htm
;;; ============================================================

;;; float: real &optional float -> float
;;; Converts to floating-point
(defcontract float
  :lambda-list (number &optional prototype)
  :argument-types ((number real)
                   (prototype (or null float)))
  :return-type float
  :postconditions ((floatp result))
  :examples
  ((float 5) => 5.0)
  ((float 1/2) => 0.5)
  ((float 3.14) => 3.14))

;;; rational: real -> rational
;;; Converts to exact rational representation
(defcontract rational
  :lambda-list (number)
  :argument-types ((number real))
  :return-type rational
  :postconditions ((rationalp result))
  :examples
  ((rational 5) => 5)
  ((rational 0.5) => 1/2)
  ((rational 0.25) => 1/4))

;;; ============================================================
;;; STRING PARSING
;;; HyperSpec: resources/HyperSpec/Body/f_parse_.htm
;;; ============================================================

;;; parse-integer: string &key -> (values integer-or-nil index)
;;; Parses an integer from a string
(defcontract parse-integer
  :lambda-list (string &key (start 0) end (radix 10) junk-allowed)
  :argument-types ((string string)
                   (start (integer 0 *))
                   (end (or null (integer 0 *)))
                   (radix (integer 2 36))
                   (junk-allowed t))
  :return-type (values (or integer null) (integer 0 *))
  :preconditions ((<= 0 start (or end (length string)))
                  (<= 2 radix 36))
  :postconditions ((or (integerp (nth-value 0 result))
                       (and junk-allowed (null (nth-value 0 result))))
                   (<= start (nth-value 1 result) (or end (length string))))
  :examples
  ((parse-integer "123") => (values 123 3))
  ((parse-integer "   123   ") => (values 123 9))
  ((parse-integer "FF" :radix 16) => (values 255 2))
  ((parse-integer "1101" :radix 2) => (values 13 4))
  ((parse-integer "+42") => (values 42 3))
  ((parse-integer "-789") => (values -789 4))
  ((parse-integer "abc" :junk-allowed t) => (values nil 0))
  ((parse-integer "123abc" :junk-allowed t) => (values 123 3))
  ((parse-integer "123" :start 1) => (values 23 3))
  ((parse-integer "123" :end 2) => (values 12 2)))

;;; ============================================================
;;; END OF CONTRACTS
;;; ============================================================
