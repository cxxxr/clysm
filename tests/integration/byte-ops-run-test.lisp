;;;; byte-ops-run-test.lisp - Integration tests for byte operations
;;;; Feature: 001-numeric-predicates (Phase 14B)
;;;;
;;;; These tests verify the complete compilation and runtime pipeline:
;;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution

(in-package #:clysm/tests/integration/byte-ops)

;;; ============================================================
;;; Bit Testing Functions (User Story 3)
;;; ============================================================

(deftest test-logbitp-bit0-set
  "logbitp: bit 0 of 5 (binary 101) is set"
  (ok (clysm/tests:compile-and-run '(logbitp 0 5))
      "(logbitp 0 5) should be T"))

(deftest test-logbitp-bit1-unset
  "logbitp: bit 1 of 5 (binary 101) is not set"
  (ok (not (clysm/tests:compile-and-run '(logbitp 1 5)))
      "(logbitp 1 5) should be NIL"))

(deftest test-logbitp-bit2-set
  "logbitp: bit 2 of 5 (binary 101) is set"
  (ok (clysm/tests:compile-and-run '(logbitp 2 5))
      "(logbitp 2 5) should be T"))

(deftest test-logbitp-high-bit-unset
  "logbitp: bit 10 of 5 is not set"
  (ok (not (clysm/tests:compile-and-run '(logbitp 10 5)))
      "(logbitp 10 5) should be NIL"))

(deftest test-logtest-common-bits
  "logtest: 5 and 7 share bits (101 & 111 = 101)"
  (ok (clysm/tests:compile-and-run '(logtest 5 7))
      "(logtest 5 7) should be T"))

(deftest test-logtest-no-common-bits
  "logtest: 4 and 3 share no bits (100 & 011 = 000)"
  (ok (not (clysm/tests:compile-and-run '(logtest 4 3)))
      "(logtest 4 3) should be NIL"))

(deftest test-logtest-zero
  "logtest: 0 and any number share no bits"
  (ok (not (clysm/tests:compile-and-run '(logtest 0 255)))
      "(logtest 0 255) should be NIL"))

;;; ============================================================
;;; Byte Specifier Functions (User Story 4)
;;; ============================================================

(deftest test-byte-size-extraction
  "byte-size extracts size from byte specifier"
  (ok (= 8 (clysm/tests:compile-and-run '(byte-size (byte 8 4))))
      "(byte-size (byte 8 4)) should be 8"))

(deftest test-byte-position-extraction
  "byte-position extracts position from byte specifier"
  (ok (= 4 (clysm/tests:compile-and-run '(byte-position (byte 8 4))))
      "(byte-position (byte 8 4)) should be 4"))

(deftest test-byte-roundtrip-size
  "byte specifier roundtrip: size preserved"
  (ok (= 16 (clysm/tests:compile-and-run '(byte-size (byte 16 0))))
      "(byte-size (byte 16 0)) should be 16"))

(deftest test-byte-roundtrip-position
  "byte specifier roundtrip: position preserved"
  (ok (= 24 (clysm/tests:compile-and-run '(byte-position (byte 8 24))))
      "(byte-position (byte 8 24)) should be 24"))

;;; ============================================================
;;; Byte Operations (User Story 5)
;;; ============================================================

(deftest test-ldb-extract-nibble
  "ldb: extract upper nibble from #xAB"
  ;; #xAB = 171, upper nibble (bits 4-7) = #xA = 10
  (ok (= 10 (clysm/tests:compile-and-run '(ldb (byte 4 4) 171)))
      "(ldb (byte 4 4) 171) should be 10"))

(deftest test-ldb-extract-lower-nibble
  "ldb: extract lower nibble from #xAB"
  ;; #xAB = 171, lower nibble (bits 0-3) = #xB = 11
  (ok (= 11 (clysm/tests:compile-and-run '(ldb (byte 4 0) 171)))
      "(ldb (byte 4 0) 171) should be 11"))

(deftest test-ldb-full-byte
  "ldb: extract full byte"
  (ok (= 255 (clysm/tests:compile-and-run '(ldb (byte 8 0) 255)))
      "(ldb (byte 8 0) 255) should be 255"))

(deftest test-mask-field-upper-nibble
  "mask-field: mask upper nibble keeping position"
  ;; #xAB = 171, mask bits 4-7 = #xA0 = 160
  (ok (= 160 (clysm/tests:compile-and-run '(mask-field (byte 4 4) 171)))
      "(mask-field (byte 4 4) 171) should be 160"))

(deftest test-mask-field-lower-nibble
  "mask-field: mask lower nibble keeping position"
  ;; #xAB = 171, mask bits 0-3 = #x0B = 11
  (ok (= 11 (clysm/tests:compile-and-run '(mask-field (byte 4 0) 171)))
      "(mask-field (byte 4 0) 171) should be 11"))

(deftest test-dpb-replace-nibble
  "dpb: deposit #xC into upper nibble of #xAB"
  ;; Replace upper nibble of #xAB (171) with #xC (12) => #xCB = 203
  (ok (= 203 (clysm/tests:compile-and-run '(dpb 12 (byte 4 4) 171)))
      "(dpb 12 (byte 4 4) 171) should be 203"))

(deftest test-dpb-clear-nibble
  "dpb: clear upper nibble by depositing 0"
  ;; Clear upper nibble of #xAB (171) => #x0B = 11
  (ok (= 11 (clysm/tests:compile-and-run '(dpb 0 (byte 4 4) 171)))
      "(dpb 0 (byte 4 4) 171) should be 11"))

(deftest test-deposit-field-in-place
  "deposit-field: deposit pre-positioned value"
  ;; Deposit #xC0 (192) into bits 4-7 of #xAB (171)
  ;; #xAB & ~#xF0 = #x0B, #xC0 & #xF0 = #xC0, result = #xCB = 203
  (ok (= 203 (clysm/tests:compile-and-run '(deposit-field 192 (byte 4 4) 171)))
      "(deposit-field 192 (byte 4 4) 171) should be 203"))

(deftest test-deposit-field-lower
  "deposit-field: deposit into lower nibble"
  ;; Deposit #x05 into bits 0-3 of #xAB (171)
  ;; #xAB & ~#x0F = #xA0, #x05 & #x0F = #x05, result = #xA5 = 165
  (ok (= 165 (clysm/tests:compile-and-run '(deposit-field 5 (byte 4 0) 171)))
      "(deposit-field 5 (byte 4 0) 171) should be 165"))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(deftest test-byte-zero-size
  "byte specifier with size 0 extracts nothing"
  (ok (= 0 (clysm/tests:compile-and-run '(ldb (byte 0 4) 255)))
      "(ldb (byte 0 4) 255) should be 0"))

(deftest test-logbitp-zero-integer
  "logbitp on zero integer is always NIL"
  (ok (not (clysm/tests:compile-and-run '(logbitp 0 0)))
      "(logbitp 0 0) should be NIL"))
