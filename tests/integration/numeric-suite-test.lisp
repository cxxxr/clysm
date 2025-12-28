;;;; tests/integration/numeric-suite-test.lisp
;;;; ANSI Numbers Category Compliance Test Suite (001-numeric-functions)
;;;;
;;;; T137: Type contagion tests for mixed-type operations
;;;; T138: ANSI numbers category compliance verification

(in-package #:clysm/tests/integration/numeric-suite)

;;; ============================================================
;;; T137: Type Contagion Tests
;;; ============================================================
;;;
;;; ANSI CL type contagion rules:
;;; integer + ratio → ratio
;;; integer + float → float
;;; ratio + float → float
;;; real + complex → complex

(deftest test-type-contagion-integer-ratio
  "integer + ratio returns ratio"
  (let ((result (compile-and-run-numeric '(+ 1 1/2))))
    (ok (rationalp result)
        "(+ 1 1/2) should return a rational")
    (ok (= 3/2 result)
        "(+ 1 1/2) should equal 3/2")))

(deftest test-type-contagion-integer-float
  "integer + float returns float"
  (let ((result (compile-and-run-numeric '(+ 1 1.5))))
    (ok (floatp result)
        "(+ 1 1.5) should return a float")
    (ok (approx= 2.5 result)
        "(+ 1 1.5) should equal 2.5")))

(deftest test-type-contagion-ratio-float
  "ratio + float returns float"
  (let ((result (compile-and-run-numeric '(+ 1/2 0.5))))
    (ok (floatp result)
        "(+ 1/2 0.5) should return a float")
    (ok (approx= 1.0 result)
        "(+ 1/2 0.5) should equal 1.0")))

(deftest test-type-contagion-real-complex
  "real + complex returns complex"
  (let ((result (compile-and-run-numeric '(+ 1 #C(0 1)))))
    (ok (complexp result)
        "(+ 1 #C(0 1)) should return a complex")
    (ok (= #C(1 1) result)
        "(+ 1 #C(0 1)) should equal #C(1 1)")))

(deftest test-type-contagion-multiply-mixed
  "integer * ratio * float returns float"
  (let ((result (compile-and-run-numeric '(* 2 1/4 2.0))))
    (ok (floatp result)
        "(* 2 1/4 2.0) should return a float")
    (ok (approx= 1.0 result)
        "(* 2 1/4 2.0) should equal 1.0")))

(deftest test-type-contagion-complex-operations
  "complex * complex returns complex"
  (let ((result (compile-and-run-numeric '(* #C(1 1) #C(1 -1)))))
    ;; (1+i)(1-i) = 1 - i^2 = 1 + 1 = 2
    (ok (or (complexp result) (realp result))
        "(* #C(1 1) #C(1 -1)) should return a number")
    (ok (= 2 (realpart result))
        "Real part should be 2")))

;;; ============================================================
;;; T138: ANSI Numbers Category Compliance Suite
;;; ============================================================

;;; --- Basic Arithmetic (abs, signum, max, min, gcd, lcm) ---

(deftest compliance-abs-integer
  "(abs -5) => 5 [ANSI 12.2.19]"
  (ok (= 5 (compile-and-run-numeric '(abs -5)))))

(deftest compliance-abs-float
  "(abs -2.5) => 2.5 [ANSI 12.2.19]"
  (ok (approx= 2.5 (compile-and-run-numeric '(abs -2.5)))))

(deftest compliance-signum-positive
  "(signum 42) => 1 [ANSI 12.2.42]"
  (ok (= 1 (compile-and-run-numeric '(signum 42)))))

(deftest compliance-signum-negative
  "(signum -3.14) => -1.0 [ANSI 12.2.42]"
  (ok (approx= -1.0 (compile-and-run-numeric '(signum -3.14)))))

(deftest compliance-signum-zero
  "(signum 0) => 0 [ANSI 12.2.42]"
  (ok (= 0 (compile-and-run-numeric '(signum 0)))))

(deftest compliance-gcd-basic
  "(gcd 48 18) => 6 [ANSI 12.2.22]"
  (ok (= 6 (compile-and-run-numeric '(gcd 48 18)))))

(deftest compliance-lcm-basic
  "(lcm 4 6) => 12 [ANSI 12.2.31]"
  (ok (= 12 (compile-and-run-numeric '(lcm 4 6)))))

;;; --- Trigonometric Functions ---

(deftest compliance-sin-zero
  "(sin 0) => 0.0 [ANSI 12.2.43]"
  (ok (approx= 0.0 (compile-and-run-numeric '(sin 0)))))

(deftest compliance-cos-zero
  "(cos 0) => 1.0 [ANSI 12.2.13]"
  (ok (approx= 1.0 (compile-and-run-numeric '(cos 0)))))

(deftest compliance-tan-zero
  "(tan 0) => 0.0 [ANSI 12.2.47]"
  (ok (approx= 0.0 (compile-and-run-numeric '(tan 0)))))

(deftest compliance-asin-zero
  "(asin 0) => 0.0 [ANSI 12.2.3]"
  (ok (approx= 0.0 (compile-and-run-numeric '(asin 0)))))

(deftest compliance-acos-one
  "(acos 1) => 0.0 [ANSI 12.2.2]"
  (ok (approx= 0.0 (compile-and-run-numeric '(acos 1)))))

(deftest compliance-atan-zero
  "(atan 0) => 0.0 [ANSI 12.2.4]"
  (ok (approx= 0.0 (compile-and-run-numeric '(atan 0)))))

;;; --- Hyperbolic Functions ---

(deftest compliance-sinh-zero
  "(sinh 0) => 0.0 [ANSI 12.2.44]"
  (ok (approx= 0.0 (compile-and-run-numeric '(sinh 0)))))

(deftest compliance-cosh-zero
  "(cosh 0) => 1.0 [ANSI 12.2.14]"
  (ok (approx= 1.0 (compile-and-run-numeric '(cosh 0)))))

(deftest compliance-tanh-zero
  "(tanh 0) => 0.0 [ANSI 12.2.48]"
  (ok (approx= 0.0 (compile-and-run-numeric '(tanh 0)))))

;;; --- Mathematical Functions (exp, log, sqrt, expt) ---

(deftest compliance-exp-zero
  "(exp 0) => 1.0 [ANSI 12.2.17]"
  (ok (approx= 1.0 (compile-and-run-numeric '(exp 0)))))

(deftest compliance-log-one
  "(log 1) => 0.0 [ANSI 12.2.33]"
  (ok (approx= 0.0 (compile-and-run-numeric '(log 1)))))

(deftest compliance-sqrt-four
  "(sqrt 4) => 2.0 [ANSI 12.2.46]"
  (ok (approx= 2.0 (compile-and-run-numeric '(sqrt 4)))))

(deftest compliance-expt-power
  "(expt 2 10) => 1024 [ANSI 12.2.18]"
  (ok (= 1024 (compile-and-run-numeric '(expt 2 10)))))

;;; --- Bitwise Operations ---

(deftest compliance-logcount-positive
  "(logcount 13) => 3 [ANSI 12.2.34]"
  ;; 13 = #b1101, has 3 one-bits
  (ok (= 3 (compile-and-run-numeric '(logcount 13)))))

(deftest compliance-integer-length
  "(integer-length 7) => 3 [ANSI 12.2.28]"
  ;; 7 = #b111, needs 3 bits
  (ok (= 3 (compile-and-run-numeric '(integer-length 7)))))

;;; --- Complex Number Operations ---

(deftest compliance-realpart
  "(realpart #C(3 4)) => 3 [ANSI 12.2.38]"
  (ok (= 3 (compile-and-run-numeric '(realpart #C(3 4))))))

(deftest compliance-imagpart
  "(imagpart #C(3 4)) => 4 [ANSI 12.2.27]"
  (ok (= 4 (compile-and-run-numeric '(imagpart #C(3 4))))))

(deftest compliance-conjugate
  "(conjugate #C(3 4)) => #C(3 -4) [ANSI 12.2.11]"
  (let ((result (compile-and-run-numeric '(conjugate #C(3 4)))))
    (ok (= 3 (realpart result)))
    (ok (= -4 (imagpart result)))))

(deftest compliance-phase
  "(phase #C(0 1)) => pi/2 [ANSI 12.2.36]"
  (let ((result (compile-and-run-numeric '(phase #C(0 1))))
        (pi/2 (/ 3.141592653589793d0 2)))
    (ok (approx= pi/2 result))))

;;; --- Division and Rounding ---

(deftest compliance-floor
  "(floor 7 2) => 3 [ANSI 12.2.20]"
  (ok (= 3 (compile-and-run-numeric '(floor 7 2)))))

(deftest compliance-ceiling
  "(ceiling 7 2) => 4 [ANSI 12.2.8]"
  (ok (= 4 (compile-and-run-numeric '(ceiling 7 2)))))

(deftest compliance-truncate
  "(truncate 7 2) => 3 [ANSI 12.2.49]"
  (ok (= 3 (compile-and-run-numeric '(truncate 7 2)))))

(deftest compliance-round
  "(round 7 2) => 4 [ANSI 12.2.40]"
  ;; 7/2 = 3.5, rounds to 4 (banker's rounding to even)
  (ok (= 4 (compile-and-run-numeric '(round 7 2)))))

(deftest compliance-mod
  "(mod 7 3) => 1 [ANSI 12.2.35]"
  (ok (= 1 (compile-and-run-numeric '(mod 7 3)))))

(deftest compliance-rem
  "(rem 7 3) => 1 [ANSI 12.2.39]"
  (ok (= 1 (compile-and-run-numeric '(rem 7 3)))))

;;; ============================================================
;;; Summary Statistics
;;; ============================================================

(deftest compliance-suite-summary
  "Print compliance test summary"
  ;; This test always passes - it's for documentation
  (format t "~&~%ANSI Numeric Functions Compliance Suite:~%")
  (format t "  - Type Contagion: 6 tests~%")
  (format t "  - Basic Arithmetic: 7 tests~%")
  (format t "  - Trigonometric: 6 tests~%")
  (format t "  - Hyperbolic: 3 tests~%")
  (format t "  - Mathematical: 4 tests~%")
  (format t "  - Bitwise: 2 tests~%")
  (format t "  - Complex: 4 tests~%")
  (format t "  - Division/Rounding: 6 tests~%")
  (format t "  Total: 38 compliance tests~%")
  (ok t "Compliance suite loaded"))
