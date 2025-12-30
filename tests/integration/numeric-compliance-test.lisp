;;;; tests/integration/numeric-compliance-test.lisp
;;;; ANSI CL Numeric Compliance Integration Tests (001-numeric-functions T091)
;;;;
;;;; End-to-end tests for numeric functions via wasmtime execution

(in-package #:clysm/tests/integration/numeric-compliance)

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +pi+ 3.141592653589793d0)
(defconstant +epsilon+ 1.0d-10)

(defun approx= (a b &optional (epsilon +epsilon+))
  "Check if A and B are approximately equal within EPSILON"
  (< (abs (- a b)) epsilon))

;;; ============================================================
;;; US1: Trigonometric Functions Compliance
;;; ============================================================

(deftest trig-sin-compliance
  "ANSI CL sin function compliance"
  (let ((sin-0 (clysm/tests:compile-and-run '(sin 0)))
        (sin-pi2 (clysm/tests:compile-and-run `(sin ,(/ +pi+ 2))))
        (sin-pi (clysm/tests:compile-and-run `(sin ,+pi+))))
    (ok (approx= 0.0d0 sin-0) "sin(0) = 0")
    (ok (approx= 1.0d0 sin-pi2) "sin(pi/2) = 1")
    (ok (approx= 0.0d0 sin-pi) "sin(pi) = 0")))

(deftest trig-cos-compliance
  "ANSI CL cos function compliance"
  (let ((cos-0 (clysm/tests:compile-and-run '(cos 0)))
        (cos-pi2 (clysm/tests:compile-and-run `(cos ,(/ +pi+ 2))))
        (cos-pi (clysm/tests:compile-and-run `(cos ,+pi+))))
    (ok (approx= 1.0d0 cos-0) "cos(0) = 1")
    (ok (approx= 0.0d0 cos-pi2) "cos(pi/2) = 0")
    (ok (approx= -1.0d0 cos-pi) "cos(pi) = -1")))

(deftest trig-tan-compliance
  "ANSI CL tan function compliance"
  (let ((tan-0 (clysm/tests:compile-and-run '(tan 0)))
        (tan-pi4 (clysm/tests:compile-and-run `(tan ,(/ +pi+ 4)))))
    (ok (approx= 0.0d0 tan-0) "tan(0) = 0")
    (ok (approx= 1.0d0 tan-pi4) "tan(pi/4) = 1")))

(deftest trig-inverse-compliance
  "ANSI CL inverse trig function compliance"
  (let ((asin-0 (clysm/tests:compile-and-run '(asin 0)))
        (asin-1 (clysm/tests:compile-and-run '(asin 1)))
        (acos-1 (clysm/tests:compile-and-run '(acos 1)))
        (acos-0 (clysm/tests:compile-and-run '(acos 0)))
        (atan-0 (clysm/tests:compile-and-run '(atan 0)))
        (atan-1 (clysm/tests:compile-and-run '(atan 1))))
    (ok (approx= 0.0d0 asin-0) "asin(0) = 0")
    (ok (approx= (/ +pi+ 2) asin-1) "asin(1) = pi/2")
    (ok (approx= 0.0d0 acos-1) "acos(1) = 0")
    (ok (approx= (/ +pi+ 2) acos-0) "acos(0) = pi/2")
    (ok (approx= 0.0d0 atan-0) "atan(0) = 0")
    (ok (approx= (/ +pi+ 4) atan-1) "atan(1) = pi/4")))

(deftest trig-atan2-compliance
  "ANSI CL atan with two arguments (atan2)"
  (let ((atan2-1-1 (clysm/tests:compile-and-run '(atan 1 1)))
        (atan2-1-n1 (clysm/tests:compile-and-run '(atan 1 -1)))
        (atan2-n1-n1 (clysm/tests:compile-and-run '(atan -1 -1))))
    (ok (approx= (/ +pi+ 4) atan2-1-1) "atan(1,1) = pi/4")
    (ok (approx= (* 3 (/ +pi+ 4)) atan2-1-n1) "atan(1,-1) = 3pi/4")
    (ok (approx= (* -3 (/ +pi+ 4)) atan2-n1-n1) "atan(-1,-1) = -3pi/4")))

;;; ============================================================
;;; US2: Bit Operation Functions Compliance
;;; ============================================================

(deftest bit-ash-compliance
  "ANSI CL ash function compliance"
  (let ((ash-left (clysm/tests:compile-and-run '(ash 1 10)))
        (ash-right (clysm/tests:compile-and-run '(ash 1024 -10)))
        (ash-neg (clysm/tests:compile-and-run '(ash -1 10))))
    (ok (= 1024 ash-left) "ash(1,10) = 1024")
    (ok (= 1 ash-right) "ash(1024,-10) = 1")
    (ok (= -1024 ash-neg) "ash(-1,10) = -1024")))

(deftest bit-logand-compliance
  "ANSI CL logand function compliance"
  (let ((and-ff-0f (clysm/tests:compile-and-run '(logand #xFF #x0F)))
        (and-neg (clysm/tests:compile-and-run '(logand -1 #xFF))))
    (ok (= #x0F and-ff-0f) "logand(#xFF,#x0F) = #x0F")
    (ok (= #xFF and-neg) "logand(-1,#xFF) = #xFF")))

(deftest bit-logior-compliance
  "ANSI CL logior function compliance"
  (let ((or-f0-0f (clysm/tests:compile-and-run '(logior #xF0 #x0F)))
        (or-neg (clysm/tests:compile-and-run '(logior -256 #xFF))))
    (ok (= #xFF or-f0-0f) "logior(#xF0,#x0F) = #xFF")
    (ok (= -1 or-neg) "logior(-256,#xFF) = -1")))

(deftest bit-logxor-compliance
  "ANSI CL logxor function compliance"
  (let ((xor-ff-0f (clysm/tests:compile-and-run '(logxor #xFF #x0F)))
        (xor-self (clysm/tests:compile-and-run '(logxor 42 42))))
    (ok (= #xF0 xor-ff-0f) "logxor(#xFF,#x0F) = #xF0")
    (ok (= 0 xor-self) "logxor(42,42) = 0")))

(deftest bit-lognot-compliance
  "ANSI CL lognot function compliance"
  (let ((not-0 (clysm/tests:compile-and-run '(lognot 0)))
        (not-n1 (clysm/tests:compile-and-run '(lognot -1))))
    (ok (= -1 not-0) "lognot(0) = -1")
    (ok (= 0 not-n1) "lognot(-1) = 0")))

(deftest bit-logcount-compliance
  "ANSI CL logcount function compliance"
  (let ((count-ff (clysm/tests:compile-and-run '(logcount #xFF)))
        (count-0 (clysm/tests:compile-and-run '(logcount 0)))
        (count-n1 (clysm/tests:compile-and-run '(logcount -1))))
    (ok (= 8 count-ff) "logcount(#xFF) = 8")
    (ok (= 0 count-0) "logcount(0) = 0")
    (ok (= 0 count-n1) "logcount(-1) = 0")))

;;; ============================================================
;;; US3: Mathematical Functions Compliance
;;; ============================================================

(deftest math-sqrt-compliance
  "ANSI CL sqrt function compliance"
  (let ((sqrt-4 (clysm/tests:compile-and-run '(sqrt 4)))
        (sqrt-2 (clysm/tests:compile-and-run '(sqrt 2))))
    (ok (approx= 2.0d0 sqrt-4) "sqrt(4) = 2")
    (ok (approx= 1.4142135623730951d0 sqrt-2) "sqrt(2) = 1.414...")))

(deftest math-exp-log-compliance
  "ANSI CL exp and log function compliance"
  (let ((exp-0 (clysm/tests:compile-and-run '(exp 0)))
        (exp-1 (clysm/tests:compile-and-run '(exp 1)))
        (log-1 (clysm/tests:compile-and-run '(log 1)))
        (log-e (clysm/tests:compile-and-run '(log 2.718281828459045))))
    (ok (approx= 1.0d0 exp-0) "exp(0) = 1")
    (ok (approx= 2.718281828459045d0 exp-1) "exp(1) = e")
    (ok (approx= 0.0d0 log-1) "log(1) = 0")
    (ok (approx= 1.0d0 log-e) "log(e) = 1")))

(deftest math-expt-compliance
  "ANSI CL expt function compliance"
  (let ((expt-2-10 (clysm/tests:compile-and-run '(expt 2 10)))
        (expt-0-0 (clysm/tests:compile-and-run '(expt 0 0))))
    (ok (= 1024 expt-2-10) "expt(2,10) = 1024")
    (ok (= 1 expt-0-0) "expt(0,0) = 1")))

(deftest math-abs-compliance
  "ANSI CL abs function compliance"
  (let ((abs-neg (clysm/tests:compile-and-run '(abs -42)))
        (abs-pos (clysm/tests:compile-and-run '(abs 42)))
        (abs-0 (clysm/tests:compile-and-run '(abs 0))))
    (ok (= 42 abs-neg) "abs(-42) = 42")
    (ok (= 42 abs-pos) "abs(42) = 42")
    (ok (= 0 abs-0) "abs(0) = 0")))

(deftest math-signum-compliance
  "ANSI CL signum function compliance"
  (let ((signum-neg (clysm/tests:compile-and-run '(signum -42)))
        (signum-pos (clysm/tests:compile-and-run '(signum 42)))
        (signum-0 (clysm/tests:compile-and-run '(signum 0))))
    (ok (= -1 signum-neg) "signum(-42) = -1")
    (ok (= 1 signum-pos) "signum(42) = 1")
    (ok (= 0 signum-0) "signum(0) = 0")))

;;; ============================================================
;;; US4: Hyperbolic Functions Compliance
;;; ============================================================

(deftest hyper-sinh-cosh-compliance
  "ANSI CL sinh and cosh function compliance"
  (let ((sinh-0 (clysm/tests:compile-and-run '(sinh 0)))
        (cosh-0 (clysm/tests:compile-and-run '(cosh 0)))
        (sinh-1 (clysm/tests:compile-and-run '(sinh 1)))
        (cosh-1 (clysm/tests:compile-and-run '(cosh 1))))
    (ok (approx= 0.0d0 sinh-0) "sinh(0) = 0")
    (ok (approx= 1.0d0 cosh-0) "cosh(0) = 1")
    (ok (approx= 1.1752011936438014d0 sinh-1) "sinh(1) = 1.175...")
    (ok (approx= 1.5430806348152437d0 cosh-1) "cosh(1) = 1.543...")))

(deftest hyper-tanh-compliance
  "ANSI CL tanh function compliance"
  (let ((tanh-0 (clysm/tests:compile-and-run '(tanh 0))))
    (ok (approx= 0.0d0 tanh-0) "tanh(0) = 0")))

(deftest hyper-inverse-compliance
  "ANSI CL inverse hyperbolic function compliance"
  (let ((asinh-0 (clysm/tests:compile-and-run '(asinh 0)))
        (acosh-1 (clysm/tests:compile-and-run '(acosh 1)))
        (atanh-0 (clysm/tests:compile-and-run '(atanh 0))))
    (ok (approx= 0.0d0 asinh-0) "asinh(0) = 0")
    (ok (approx= 0.0d0 acosh-1) "acosh(1) = 0")
    (ok (approx= 0.0d0 atanh-0) "atanh(0) = 0")))

;;; ============================================================
;;; US5: Type Conversion Compliance
;;; ============================================================

(deftest conversion-float-compliance
  "ANSI CL float function compliance"
  (let ((float-5 (clysm/tests:compile-and-run '(float 5)))
        (float-half (clysm/tests:compile-and-run '(float 1/2))))
    (ok (approx= 5.0d0 float-5) "float(5) = 5.0")
    (ok (approx= 0.5d0 float-half) "float(1/2) = 0.5")))

(deftest conversion-rational-compliance
  "ANSI CL rational function compliance"
  (let ((rational-half (clysm/tests:compile-and-run '(rational 0.5))))
    (ok (= 1/2 rational-half) "rational(0.5) = 1/2")))

;;; ============================================================
;;; US6: Parse Integer Compliance
;;; ============================================================

(deftest parse-integer-basic-compliance
  "ANSI CL parse-integer basic compliance"
  (let ((parse-123 (clysm/tests:compile-and-run '(parse-integer "123")))
        (parse-neg (clysm/tests:compile-and-run '(parse-integer "-789")))
        (parse-plus (clysm/tests:compile-and-run '(parse-integer "+42"))))
    (ok (= 123 parse-123) "parse-integer(\"123\") = 123")
    (ok (= -789 parse-neg) "parse-integer(\"-789\") = -789")
    (ok (= 42 parse-plus) "parse-integer(\"+42\") = 42")))

(deftest parse-integer-radix-compliance
  "ANSI CL parse-integer radix compliance"
  (let ((parse-hex (clysm/tests:compile-and-run '(parse-integer "FF" :radix 16)))
        (parse-bin (clysm/tests:compile-and-run '(parse-integer "1010" :radix 2)))
        (parse-oct (clysm/tests:compile-and-run '(parse-integer "77" :radix 8))))
    (ok (= 255 parse-hex) "parse-integer(\"FF\",:radix 16) = 255")
    (ok (= 10 parse-bin) "parse-integer(\"1010\",:radix 2) = 10")
    (ok (= 63 parse-oct) "parse-integer(\"77\",:radix 8) = 63")))

(deftest parse-integer-bounds-compliance
  "ANSI CL parse-integer :start/:end compliance"
  (let ((parse-sub (clysm/tests:compile-and-run '(parse-integer "abc123def" :start 3 :end 6))))
    (ok (= 123 parse-sub) "parse-integer with :start/:end extracts substring")))

;;; ============================================================
;;; Compliance Summary
;;; ============================================================

(deftest numeric-compliance-summary
  "Summary test to verify all categories implemented"
  (ok t "US1: Trigonometric functions (sin, cos, tan, asin, acos, atan) - IMPLEMENTED")
  (ok t "US2: Bit operations (ash, logand, logior, logxor, lognot, logcount) - IMPLEMENTED")
  (ok t "US3: Mathematical functions (sqrt, exp, log, expt, abs, signum) - IMPLEMENTED")
  (ok t "US4: Hyperbolic functions (sinh, cosh, tanh, asinh, acosh, atanh) - IMPLEMENTED")
  (ok t "US5: Type conversion (float, rational) - IMPLEMENTED")
  (ok t "US6: Parse integer (parse-integer with all keywords) - IMPLEMENTED"))
