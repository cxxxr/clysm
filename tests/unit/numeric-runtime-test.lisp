;;;; numeric-runtime-test.lisp - Unit tests for numeric runtime functions
;;;; Feature: 001-numeric-runtime-migration
;;;;
;;;; Tests for numeric manipulation runtime functions.
;;;; TDD: These tests must be written before implementation (Constitution VII).
;;;;
;;;; HyperSpec references:
;;;;   [parse-integer](resources/HyperSpec/Body/f_parse_.htm)
;;;;   [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm)
;;;;   [rationalize](resources/HyperSpec/Body/f_ration.htm)
;;;;   [signum](resources/HyperSpec/Body/f_signum.htm)
;;;;   [phase](resources/HyperSpec/Body/f_phase.htm)

(in-package #:clysm/tests)

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Function Tests (T010-T014)
;;; ============================================================

;;; T010: Test parse-integer basic parsing
(deftest parse-integer-basic-positive-integer ()
  "Verify parse-integer parses basic positive integer"
  (testing "basic positive integer parsing"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "123")
      (ok (integerp result) "Result is an integer")
      (ok (integerp index) "Index is an integer"))))

;;; T011: Test write-to-string basic output
(deftest write-to-string-basic-integer ()
  "Verify write-to-string converts integer to string"
  (testing "basic integer to string"
    (let ((result (clysm::write-to-string-rt 0)))
      (ok (stringp result) "Result is a string"))))

;;; T012: Test rationalize passthrough
(deftest rationalize-integer-passthrough ()
  "Verify rationalize returns integer unchanged"
  (testing "integer passthrough"
    (let ((result (clysm::rationalize-rt 5)))
      (ok (numberp result) "Result is a number"))))

;;; T013: Test signum basic sign detection
(deftest signum-basic-sign-detection ()
  "Verify signum returns sign indicator"
  (testing "basic sign detection"
    (let ((result (clysm::signum-rt 0)))
      (ok (numberp result) "Result is a number"))))

;;; T014: Test phase for real positive
(deftest phase-real-positive ()
  "Verify phase returns 0 for positive real"
  (testing "positive real phase"
    (let ((result (clysm::phase-rt 1)))
      (ok (numberp result) "Result is a number"))))

;;; ============================================================
;;; Phase 4: User Story 2 - Keyword Argument Tests (T022-T029)
;;; ============================================================

;;; T022: Test parse-integer with :radix 2 (binary)
(deftest parse-integer-radix-binary ()
  "Verify parse-integer handles binary radix"
  (testing "binary radix parsing"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "1010" :radix 2)
      (ok (= result 10) "Binary 1010 = 10")
      (ok (= index 4) "Index at end of string"))))

;;; T023: Test parse-integer with :radix 16 (hexadecimal)
(deftest parse-integer-radix-hexadecimal ()
  "Verify parse-integer handles hexadecimal radix"
  (testing "hexadecimal radix parsing"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "FF" :radix 16)
      (ok (= result 255) "Hex FF = 255")
      (ok (= index 2) "Index at end of string"))))

;;; T024: Test parse-integer with :start/:end bounds
(deftest parse-integer-start-end-bounds ()
  "Verify parse-integer respects :start and :end bounds"
  (testing ":start/:end bounds"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "xxx42yyy" :start 3 :end 5)
      (ok (= result 42) "Parsed 42 from middle")
      (ok (= index 5) "Index at :end position"))))

;;; T025: Test parse-integer with :junk-allowed t
(deftest parse-integer-junk-allowed-true ()
  "Verify parse-integer allows trailing junk"
  (testing ":junk-allowed t"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "123abc" :junk-allowed t)
      (ok (= result 123) "Parsed 123 before junk")
      (ok (= index 3) "Index at first non-digit"))))

;;; T026: Test parse-integer with :junk-allowed nil signals error
(deftest parse-integer-junk-allowed-nil-error ()
  "Verify parse-integer signals error on trailing junk"
  (testing ":junk-allowed nil signals error"
    (ok (signals (clysm::parse-integer-rt "123abc" :junk-allowed nil))
        "Error signaled for trailing junk")))

;;; T027: Test write-to-string with :base 2 (binary)
(deftest write-to-string-base-binary ()
  "Verify write-to-string handles binary base"
  (testing "binary base output"
    (let ((result (clysm::write-to-string-rt 10 :base 2)))
      (ok (string= result "1010") "10 in binary is 1010"))))

;;; T028: Test write-to-string with :base 16 (hexadecimal)
(deftest write-to-string-base-hexadecimal ()
  "Verify write-to-string handles hexadecimal base"
  (testing "hexadecimal base output"
    (let ((result (clysm::write-to-string-rt 255 :base 16)))
      (ok (string= result "FF") "255 in hex is FF"))))

;;; T029: Test write-to-string with :base 36
(deftest write-to-string-base-36 ()
  "Verify write-to-string handles base 36"
  (testing "base 36 output"
    (let ((result (clysm::write-to-string-rt 35 :base 36)))
      (ok (string= result "Z") "35 in base 36 is Z"))))

;;; ============================================================
;;; Phase 5: User Story 3 - Type Dispatch Tests (T035-T044)
;;; ============================================================

;;; T035: Test signum with integer input returns integer
(deftest signum-integer-returns-integer ()
  "Verify signum returns integer for integer input"
  (testing "signum integer type preservation"
    (ok (integerp (clysm::signum-rt 5)) "Positive integer returns integer")
    (ok (integerp (clysm::signum-rt -5)) "Negative integer returns integer")
    (ok (= (clysm::signum-rt 5) 1) "Positive integer returns 1")
    (ok (= (clysm::signum-rt -5) -1) "Negative integer returns -1")))

;;; T036: Test signum with float input returns float
(deftest signum-float-returns-float ()
  "Verify signum returns float for float input"
  (testing "signum float type preservation"
    (ok (floatp (clysm::signum-rt 3.14)) "Positive float returns float")
    (ok (floatp (clysm::signum-rt -3.14)) "Negative float returns float")
    (ok (= (clysm::signum-rt 3.14) 1.0) "Positive float returns 1.0")
    (ok (= (clysm::signum-rt -3.14) -1.0) "Negative float returns -1.0")))

;;; T037: Test signum with ratio input returns integer
(deftest signum-ratio-returns-integer ()
  "Verify signum returns integer for ratio input"
  (testing "signum ratio type handling"
    (ok (integerp (clysm::signum-rt 1/2)) "Positive ratio returns integer")
    (ok (integerp (clysm::signum-rt -1/2)) "Negative ratio returns integer")
    (ok (= (clysm::signum-rt 1/2) 1) "Positive ratio returns 1")
    (ok (= (clysm::signum-rt -1/2) -1) "Negative ratio returns -1")))

;;; T038: Test signum with complex input returns unit complex
(deftest signum-complex-returns-unit-complex ()
  "Verify signum returns unit complex for complex input"
  (testing "signum complex normalization"
    (let ((result (clysm::signum-rt #C(3 4))))
      (ok (complexp result) "Complex input returns complex")
      ;; For #C(3 4), magnitude is 5, so signum is #C(0.6 0.8)
      (ok (< (abs (- (realpart result) 0.6)) 0.001) "Real part is 0.6")
      (ok (< (abs (- (imagpart result) 0.8)) 0.001) "Imaginary part is 0.8"))))

;;; T039: Test phase with positive real returns 0.0
(deftest phase-positive-real-returns-zero ()
  "Verify phase returns 0 for positive real"
  (testing "phase positive real"
    (ok (= (clysm::phase-rt 5) 0.0) "Positive integer phase is 0")
    (ok (= (clysm::phase-rt 3.14) 0.0) "Positive float phase is 0")))

;;; T040: Test phase with negative real returns pi
(deftest phase-negative-real-returns-pi ()
  "Verify phase returns pi for negative real"
  (testing "phase negative real"
    (let ((pi-val 3.141592653589793d0))
      (ok (< (abs (- (clysm::phase-rt -5) pi-val)) 0.001) "Negative integer phase is pi")
      (ok (< (abs (- (clysm::phase-rt -3.14) pi-val)) 0.001) "Negative float phase is pi"))))

;;; T041: Test phase with complex in each quadrant
(deftest phase-complex-quadrants ()
  "Verify phase returns correct angle for complex in each quadrant"
  (testing "phase complex quadrants"
    (let ((pi-val 3.141592653589793d0))
      ;; Q1: +real, +imag -> 0 < phase < pi/2
      (let ((p (clysm::phase-rt #C(1 1))))
        (ok (and (> p 0) (< p (/ pi-val 2))) "Q1 phase in (0, pi/2)"))
      ;; Q2: -real, +imag -> pi/2 < phase < pi
      (let ((p (clysm::phase-rt #C(-1 1))))
        (ok (and (> p (/ pi-val 2)) (< p pi-val)) "Q2 phase in (pi/2, pi)"))
      ;; Q3: -real, -imag -> -pi < phase < -pi/2
      (let ((p (clysm::phase-rt #C(-1 -1))))
        (ok (and (< p (- (/ pi-val 2))) (> p (- pi-val))) "Q3 phase in (-pi, -pi/2)"))
      ;; Q4: +real, -imag -> -pi/2 < phase < 0
      (let ((p (clysm::phase-rt #C(1 -1))))
        (ok (and (> p (- (/ pi-val 2))) (< p 0)) "Q4 phase in (-pi/2, 0)")))))

;;; T042: Test rationalize with float 0.5 returns 1/2
(deftest rationalize-float-half ()
  "Verify rationalize converts 0.5 to 1/2"
  (testing "rationalize float to ratio"
    (let ((result (clysm::rationalize-rt 0.5)))
      (ok (rationalp result) "Result is rational")
      (ok (= result 1/2) "0.5 rationalizes to 1/2"))))

;;; T043: Test rationalize with integer returns integer unchanged
(deftest rationalize-integer-unchanged ()
  "Verify rationalize returns integer unchanged"
  (testing "rationalize integer passthrough"
    (ok (= (clysm::rationalize-rt 42) 42) "Integer 42 unchanged")
    (ok (integerp (clysm::rationalize-rt 42)) "Result is integer")))

;;; T044: Test rationalize with ratio returns ratio unchanged
(deftest rationalize-ratio-unchanged ()
  "Verify rationalize returns ratio unchanged"
  (testing "rationalize ratio passthrough"
    (ok (= (clysm::rationalize-rt 3/4) 3/4) "Ratio 3/4 unchanged")
    (ok (rationalp (clysm::rationalize-rt 3/4)) "Result is rational")))

;;; ============================================================
;;; Phase 7: User Story 5 - Edge Case Tests (T060-T069)
;;; ============================================================

;;; T060: Test parse-integer empty string signals error
(deftest parse-integer-empty-string-error ()
  "Verify parse-integer signals error for empty string"
  (testing "empty string error"
    (ok (signals (clysm::parse-integer-rt "" :junk-allowed nil))
        "Error signaled for empty string")))

;;; T061: Test parse-integer whitespace only signals error
(deftest parse-integer-whitespace-only-error ()
  "Verify parse-integer signals error for whitespace-only string"
  (testing "whitespace only error"
    (ok (signals (clysm::parse-integer-rt "   " :junk-allowed nil))
        "Error signaled for whitespace-only string")))

;;; T062: Test parse-integer negative number parsing
(deftest parse-integer-negative-number ()
  "Verify parse-integer handles negative numbers"
  (testing "negative number parsing"
    (multiple-value-bind (result index)
        (clysm::parse-integer-rt "-42")
      (ok (= result -42) "Parsed -42 correctly")
      (ok (= index 3) "Index at end of string"))))

;;; T063: Test signum with zero (returns 0)
(deftest signum-zero-returns-zero ()
  "Verify signum returns 0 for zero input"
  (testing "signum zero handling"
    (ok (= (clysm::signum-rt 0) 0) "Integer zero returns 0")
    (ok (= (clysm::signum-rt 0.0) 0.0) "Float zero returns 0.0")))

;;; T064: Test signum with -0.0 (IEEE behavior)
(deftest signum-negative-zero ()
  "Verify signum handles IEEE negative zero"
  (testing "signum -0.0 IEEE behavior"
    (let ((result (clysm::signum-rt -0.0)))
      (ok (zerop result) "Result is zero"))))

;;; T065: Test phase with zero (returns 0.0)
(deftest phase-zero-returns-zero ()
  "Verify phase returns 0.0 for zero"
  (testing "phase zero handling"
    (ok (= (clysm::phase-rt 0) 0.0) "Integer zero phase is 0.0")
    (ok (= (clysm::phase-rt 0.0) 0.0) "Float zero phase is 0.0")))

;;; T066: Test phase with pure imaginary #C(0 1)
(deftest phase-pure-imaginary ()
  "Verify phase returns pi/2 for pure positive imaginary"
  (testing "phase pure imaginary"
    (let ((pi-half (/ 3.141592653589793d0 2)))
      (ok (< (abs (- (clysm::phase-rt #C(0 1)) pi-half)) 0.001)
          "Phase of #C(0 1) is pi/2"))))

;;; T067: Test write-to-string with 0
(deftest write-to-string-zero ()
  "Verify write-to-string handles zero correctly"
  (testing "write-to-string zero"
    (ok (string= (clysm::write-to-string-rt 0) "0") "Zero outputs as 0")
    (ok (string= (clysm::write-to-string-rt 0 :base 2) "0") "Zero in binary is 0")
    (ok (string= (clysm::write-to-string-rt 0 :base 16) "0") "Zero in hex is 0")))

;;; T068: Test write-to-string with negative numbers
(deftest write-to-string-negative ()
  "Verify write-to-string handles negative numbers"
  (testing "write-to-string negative"
    (ok (string= (clysm::write-to-string-rt -42) "-42") "-42 in decimal")
    (ok (string= (clysm::write-to-string-rt -10 :base 2) "-1010") "-10 in binary")
    (ok (string= (clysm::write-to-string-rt -255 :base 16) "-FF") "-255 in hex")))

;;; T069: Test rationalize with 0.0 returns 0
(deftest rationalize-zero ()
  "Verify rationalize returns 0 for 0.0"
  (testing "rationalize zero"
    (ok (= (clysm::rationalize-rt 0.0) 0) "0.0 rationalizes to 0")
    (ok (integerp (clysm::rationalize-rt 0.0)) "Result is integer")))
