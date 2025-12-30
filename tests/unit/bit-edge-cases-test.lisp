;;;; tests/unit/bit-edge-cases-test.lisp
;;;; Edge case tests for negative numbers in bit operations (001-numeric-functions T090)
;;;;
;;;; Tests for ash, logand, logior, logxor, lognot with negative inputs

(in-package #:clysm/tests/unit/bit-edge-cases)

;;; ============================================================
;;; ASH with Negative Inputs
;;; ============================================================

(deftest test-ash-negative-value-left
  "ash left shift of negative value: (ash -1 10) => -1024"
  (ok (= -1024 (compile-and-run '(ash -1 10)))
      "(ash -1 10) should equal -1024"))

(deftest test-ash-negative-value-right
  "ash right shift of negative value: (ash -1024 -10) => -1"
  (ok (= -1 (compile-and-run '(ash -1024 -10)))
      "(ash -1024 -10) should equal -1"))

(deftest test-ash-negative-value-large-shift
  "ash left shift of negative value: (ash -1 20) => -1048576"
  (ok (= -1048576 (compile-and-run '(ash -1 20)))
      "(ash -1 20) should equal -1048576"))

(deftest test-ash-negative-right-shift-sign-extension
  "ash right shift preserves sign: (ash -8 -2) => -2"
  (ok (= -2 (compile-and-run '(ash -8 -2)))
      "(ash -8 -2) should equal -2"))

(deftest test-ash-negative-right-shift-small
  "ash right shift of -4: (ash -4 -1) => -2"
  (ok (= -2 (compile-and-run '(ash -4 -1)))
      "(ash -4 -1) should equal -2"))

;;; ============================================================
;;; LOGAND with Negative Inputs
;;; ============================================================

(deftest test-logand-negative-and-positive
  "logand of negative and positive: (logand -1 #xFF) => 255"
  (ok (= 255 (compile-and-run '(logand -1 #xFF)))
      "(logand -1 #xFF) should equal 255"))

(deftest test-logand-two-negatives
  "logand of two negatives: (logand -2 -4) => -4"
  (ok (= -4 (compile-and-run '(logand -2 -4)))
      "(logand -2 -4) should equal -4"))

(deftest test-logand-negative-mask
  "logand with negative mask: (logand #xFFFF -256) => #xFF00"
  (ok (= #xFF00 (compile-and-run '(logand #xFFFF -256)))
      "(logand #xFFFF -256) should equal #xFF00"))

;;; ============================================================
;;; LOGIOR with Negative Inputs
;;; ============================================================

(deftest test-logior-negative-and-positive
  "logior of negative and positive: (logior -256 #xFF) => -1"
  (ok (= -1 (compile-and-run '(logior -256 #xFF)))
      "(logior -256 #xFF) should equal -1"))

(deftest test-logior-two-negatives
  "logior of two negatives: (logior -2 -4) => -2"
  (ok (= -2 (compile-and-run '(logior -2 -4)))
      "(logior -2 -4) should equal -2"))

(deftest test-logior-negative-zero
  "logior of negative and zero: (logior -1 0) => -1"
  (ok (= -1 (compile-and-run '(logior -1 0)))
      "(logior -1 0) should equal -1"))

;;; ============================================================
;;; LOGXOR with Negative Inputs
;;; ============================================================

(deftest test-logxor-negative-and-positive
  "logxor of negative and positive: (logxor -1 #xFF) => -256"
  (ok (= -256 (compile-and-run '(logxor -1 #xFF)))
      "(logxor -1 #xFF) should equal -256"))

(deftest test-logxor-two-negatives
  "logxor of two negatives: (logxor -1 -1) => 0"
  (ok (= 0 (compile-and-run '(logxor -1 -1)))
      "(logxor -1 -1) should equal 0"))

(deftest test-logxor-negative-self
  "logxor of negative with itself: (logxor -42 -42) => 0"
  (ok (= 0 (compile-and-run '(logxor -42 -42)))
      "(logxor -42 -42) should equal 0"))

(deftest test-logxor-flip-sign
  "logxor to flip bits: (logxor -1 #x7FFFFFFF) => -2147483648"
  ;; -1 XOR max-signed-32 gives min-signed-32
  (ok (= -2147483648 (compile-and-run '(logxor -1 #x7FFFFFFF)))
      "(logxor -1 #x7FFFFFFF) should equal -2147483648"))

;;; ============================================================
;;; LOGNOT with Various Negatives
;;; ============================================================

(deftest test-lognot-minus-one
  "lognot of -1: (lognot -1) => 0"
  (ok (= 0 (compile-and-run '(lognot -1)))
      "(lognot -1) should equal 0"))

(deftest test-lognot-minus-two
  "lognot of -2: (lognot -2) => 1"
  (ok (= 1 (compile-and-run '(lognot -2)))
      "(lognot -2) should equal 1"))

(deftest test-lognot-minus-256
  "lognot of -256: (lognot -256) => 255"
  (ok (= 255 (compile-and-run '(lognot -256)))
      "(lognot -256) should equal 255"))

(deftest test-lognot-large-negative
  "lognot of large negative: (lognot -1000000) => 999999"
  (ok (= 999999 (compile-and-run '(lognot -1000000)))
      "(lognot -1000000) should equal 999999"))

;;; ============================================================
;;; LOGCOUNT with Various Negatives
;;; ============================================================

(deftest test-logcount-minus-two
  "logcount of -2: (logcount -2) => 1"
  (ok (= 1 (compile-and-run '(logcount -2)))
      "(logcount -2) should equal 1 (one 0-bit)"))

(deftest test-logcount-minus-three
  "logcount of -3: (logcount -3) => 1"
  (ok (= 1 (compile-and-run '(logcount -3)))
      "(logcount -3) should equal 1"))

(deftest test-logcount-minus-256
  "logcount of -256: (logcount -256) => 8"
  (ok (= 8 (compile-and-run '(logcount -256)))
      "(logcount -256) should equal 8"))

;;; ============================================================
;;; Boundary Value Tests
;;; ============================================================

(deftest test-ash-min-fixnum
  "ash with minimum fixnum boundary"
  (let ((result (compile-and-run '(ash -1073741824 1))))
    (ok (= -2147483648 result)
        "(ash -1073741824 1) should equal -2147483648")))

(deftest test-logand-min-fixnum
  "logand with minimum fixnum"
  (let ((result (compile-and-run '(logand -2147483648 -1))))
    (ok (= -2147483648 result)
        "(logand -2147483648 -1) should equal -2147483648")))
