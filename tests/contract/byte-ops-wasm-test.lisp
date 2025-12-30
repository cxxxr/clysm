;;;; tests/contract/byte-ops-wasm-test.lisp
;;;; Contract tests for Phase 14B: Numeric Type Predicates Enhancement
;;;; Validates that byte operations produce valid Wasm bytecode
;;;; See: specs/001-numeric-predicates/spec.md

(in-package #:clysm/tests/contract/byte-ops-wasm)

;;; ============================================================
;;; User Story 3: Bit Testing Functions (logbitp, logtest)
;;; Validates Wasm output for bit testing operations
;;; ============================================================

(deftest test-logbitp-wasm-valid
  "logbitp should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(logbitp 0 5))))
    (ok (validate-wasm-silent bytes)
        "(logbitp 0 5) should produce valid Wasm")))

(deftest test-logbitp-variable-index-wasm-valid
  "logbitp with variable index should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(let ((i 2)) (logbitp i 5)))))
    (ok (validate-wasm-silent bytes)
        "(let ((i 2)) (logbitp i 5)) should produce valid Wasm")))

(deftest test-logtest-wasm-valid
  "logtest should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(logtest 5 3))))
    (ok (validate-wasm-silent bytes)
        "(logtest 5 3) should produce valid Wasm")))

(deftest test-logtest-variable-wasm-valid
  "logtest with variables should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(let ((a 5) (b 3)) (logtest a b)))))
    (ok (validate-wasm-silent bytes)
        "(let ((a 5) (b 3)) (logtest a b)) should produce valid Wasm")))

;;; ============================================================
;;; User Story 4: Byte Specifier Functions (byte, byte-size, byte-position)
;;; Validates Wasm output for byte specifier operations
;;; ============================================================

(deftest test-byte-constructor-wasm-valid
  "byte constructor should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(byte 8 4))))
    (ok (validate-wasm-silent bytes)
        "(byte 8 4) should produce valid Wasm")))

(deftest test-byte-size-wasm-valid
  "byte-size should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(byte-size (byte 8 4)))))
    (ok (validate-wasm-silent bytes)
        "(byte-size (byte 8 4)) should produce valid Wasm")))

(deftest test-byte-position-wasm-valid
  "byte-position should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(byte-position (byte 8 4)))))
    (ok (validate-wasm-silent bytes)
        "(byte-position (byte 8 4)) should produce valid Wasm")))

;;; ============================================================
;;; User Story 5: Byte Extraction and Manipulation (ldb, dpb, mask-field, deposit-field)
;;; Validates Wasm output for byte manipulation operations
;;; ============================================================

(deftest test-ldb-wasm-valid
  "ldb should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(ldb (byte 4 4) #xAB))))
    (ok (validate-wasm-silent bytes)
        "(ldb (byte 4 4) #xAB) should produce valid Wasm")))

(deftest test-ldb-variable-wasm-valid
  "ldb with variable integer should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(let ((x #xAB)) (ldb (byte 4 4) x)))))
    (ok (validate-wasm-silent bytes)
        "(let ((x #xAB)) (ldb (byte 4 4) x)) should produce valid Wasm")))

(deftest test-dpb-wasm-valid
  "dpb should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(dpb #xC (byte 4 4) #xAB))))
    (ok (validate-wasm-silent bytes)
        "(dpb #xC (byte 4 4) #xAB) should produce valid Wasm")))

(deftest test-dpb-variable-wasm-valid
  "dpb with variables should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(let ((newbyte #xC) (x #xAB)) (dpb newbyte (byte 4 4) x)))))
    (ok (validate-wasm-silent bytes)
        "(let ((newbyte #xC) (x #xAB)) (dpb newbyte (byte 4 4) x)) should produce valid Wasm")))

(deftest test-mask-field-wasm-valid
  "mask-field should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(mask-field (byte 4 4) #xAB))))
    (ok (validate-wasm-silent bytes)
        "(mask-field (byte 4 4) #xAB) should produce valid Wasm")))

(deftest test-deposit-field-wasm-valid
  "deposit-field should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm '(deposit-field #xC0 (byte 4 4) #xAB))))
    (ok (validate-wasm-silent bytes)
        "(deposit-field #xC0 (byte 4 4) #xAB) should produce valid Wasm")))

;;; ============================================================
;;; Cross-cutting: Combined operations
;;; Validates complex expressions using multiple byte operations
;;; ============================================================

(deftest test-nested-byte-ops-wasm-valid
  "Nested byte operations should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm
                '(dpb (ldb (byte 4 0) #xAB) (byte 4 4) 0))))
    (ok (validate-wasm-silent bytes)
        "Nested (dpb (ldb ...)) should produce valid Wasm")))

(deftest test-byte-ops-in-function-wasm-valid
  "Byte operations in function should compile to valid Wasm"
  (let ((bytes (clysm/compiler:compile-to-wasm
                '(progn
                   (defun swap-nibbles (x)
                     (let ((low (ldb (byte 4 0) x))
                           (high (ldb (byte 4 4) x)))
                       (dpb low (byte 4 4) (dpb high (byte 4 0) 0))))
                   (swap-nibbles #xAB)))))
    (ok (validate-wasm-silent bytes)
        "Function using byte operations should produce valid Wasm")))
