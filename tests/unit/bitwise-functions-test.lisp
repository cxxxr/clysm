;;;; tests/unit/bitwise-functions-test.lisp
;;;; Unit tests for bitwise operations (001-numeric-functions US3)
;;;;
;;;; Tests for logcount, integer-length
;;;; Note: ash, logand, logior, logxor, lognot are already implemented

(in-package #:clysm/tests/unit/bitwise-functions)

;;; ============================================================
;;; Existing Bitwise Functions Tests (verification)
;;; ============================================================

(deftest test-ash-left
  "ash left shift: (ash 1 10) => 1024"
  (ok (= 1024 (compile-and-run '(ash 1 10)))
      "(ash 1 10) should equal 1024"))

(deftest test-ash-right
  "ash right shift: (ash 1024 -10) => 1"
  (ok (= 1 (compile-and-run '(ash 1024 -10)))
      "(ash 1024 -10) should equal 1"))

(deftest test-logand-basic
  "logand: (logand #xFF00 #x0FF0) => #x0F00"
  (ok (= #x0F00 (compile-and-run '(logand #xFF00 #x0FF0)))
      "(logand #xFF00 #x0FF0) should equal #x0F00"))

(deftest test-logior-basic
  "logior: (logior #xFF00 #x00FF) => #xFFFF"
  (ok (= #xFFFF (compile-and-run '(logior #xFF00 #x00FF)))
      "(logior #xFF00 #x00FF) should equal #xFFFF"))

(deftest test-logxor-basic
  "logxor: (logxor #xFF00 #x0FF0) => #xF0F0"
  (ok (= #xF0F0 (compile-and-run '(logxor #xFF00 #x0FF0)))
      "(logxor #xFF00 #x0FF0) should equal #xF0F0"))

(deftest test-lognot-basic
  "lognot: (lognot 0) => -1"
  (ok (= -1 (compile-and-run '(lognot 0)))
      "(lognot 0) should equal -1"))

;;; ============================================================
;;; Logcount Tests (FR-024)
;;; ============================================================

(deftest test-logcount-positive
  "logcount of positive: (logcount 13) => 3 (binary 1101)"
  (ok (= 3 (compile-and-run '(logcount 13)))
      "(logcount 13) should equal 3 (three 1-bits)"))

(deftest test-logcount-power-of-two
  "logcount of power of two: (logcount 1024) => 1"
  (ok (= 1 (compile-and-run '(logcount 1024)))
      "(logcount 1024) should equal 1"))

(deftest test-logcount-all-ones
  "logcount of all ones: (logcount #xFF) => 8"
  (ok (= 8 (compile-and-run '(logcount #xFF)))
      "(logcount #xFF) should equal 8"))

(deftest test-logcount-zero
  "logcount of zero: (logcount 0) => 0"
  (ok (= 0 (compile-and-run '(logcount 0)))
      "(logcount 0) should equal 0"))

(deftest test-logcount-negative
  "logcount of negative: (logcount -1) => 0 (counts 0-bits)"
  ;; For negative numbers, logcount counts 0-bits
  (ok (= 0 (compile-and-run '(logcount -1)))
      "(logcount -1) should equal 0 (no 0-bits in -1)"))

(deftest test-logcount-negative-with-zeros
  "logcount of -2: (logcount -2) => 1 (binary ...1110)"
  ;; -2 in two's complement has one 0-bit
  (ok (= 1 (compile-and-run '(logcount -2)))
      "(logcount -2) should equal 1"))

;;; ============================================================
;;; Integer-Length Tests (FR-025)
;;; ============================================================

(deftest test-integer-length-zero
  "integer-length of zero: (integer-length 0) => 0"
  (ok (= 0 (compile-and-run '(integer-length 0)))
      "(integer-length 0) should equal 0"))

(deftest test-integer-length-one
  "integer-length of one: (integer-length 1) => 1"
  (ok (= 1 (compile-and-run '(integer-length 1)))
      "(integer-length 1) should equal 1"))

(deftest test-integer-length-power-of-two
  "integer-length of 1024: (integer-length 1024) => 11"
  (ok (= 11 (compile-and-run '(integer-length 1024)))
      "(integer-length 1024) should equal 11"))

(deftest test-integer-length-255
  "integer-length of 255: (integer-length 255) => 8"
  (ok (= 8 (compile-and-run '(integer-length 255)))
      "(integer-length 255) should equal 8"))

(deftest test-integer-length-256
  "integer-length of 256: (integer-length 256) => 9"
  (ok (= 9 (compile-and-run '(integer-length 256)))
      "(integer-length 256) should equal 9"))

(deftest test-integer-length-negative
  "integer-length of negative: (integer-length -1) => 0"
  (ok (= 0 (compile-and-run '(integer-length -1)))
      "(integer-length -1) should equal 0"))

(deftest test-integer-length-negative-two
  "integer-length of -2: (integer-length -2) => 1"
  (ok (= 1 (compile-and-run '(integer-length -2)))
      "(integer-length -2) should equal 1"))

(deftest test-integer-length-large
  "integer-length of 2^30: (integer-length (ash 1 30)) => 31"
  (ok (= 31 (compile-and-run '(integer-length (ash 1 30))))
      "(integer-length 2^30) should equal 31"))
