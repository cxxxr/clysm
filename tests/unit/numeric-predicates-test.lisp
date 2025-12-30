;;;; tests/unit/numeric-predicates-test.lisp
;;;; Unit tests for numeric type predicates (010-numeric-tower Phase 8)

(in-package #:clysm/tests/unit/numeric-predicates)

;;; numberp tests

(deftest test-numberp-fixnum
  "numberp on fixnum: (numberp 42) => T"
  (ok (clysm/tests:compile-and-run '(numberp 42))
      "(numberp 42) should be T"))

(deftest test-numberp-bignum
  "numberp on bignum: (numberp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(numberp 10000000000000))
      "(numberp 10000000000000) should be T"))

(deftest test-numberp-ratio
  "numberp on ratio: (numberp 1/2) => T"
  (ok (clysm/tests:compile-and-run '(numberp 1/2))
      "(numberp 1/2) should be T"))

(deftest test-numberp-float
  "numberp on float: (numberp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(numberp 3.14))
      "(numberp 3.14) should be T"))

(deftest test-numberp-complex
  "numberp on complex: (numberp #C(1 2)) => T"
  (ok (clysm/tests:compile-and-run '(numberp #C(1 2)))
      "(numberp #C(1 2)) should be T"))

(deftest test-numberp-non-number
  "numberp on non-number: (numberp nil) => NIL"
  (ok (not (clysm/tests:compile-and-run '(numberp nil)))
      "(numberp nil) should be NIL"))

;;; integerp tests

(deftest test-integerp-fixnum
  "integerp on fixnum: (integerp 42) => T"
  (ok (clysm/tests:compile-and-run '(integerp 42))
      "(integerp 42) should be T"))

(deftest test-integerp-bignum
  "integerp on bignum: (integerp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(integerp 10000000000000))
      "(integerp 10000000000000) should be T"))

(deftest test-integerp-ratio
  "integerp on ratio: (integerp 1/2) => NIL"
  (ok (not (clysm/tests:compile-and-run '(integerp 1/2)))
      "(integerp 1/2) should be NIL"))

(deftest test-integerp-float
  "integerp on float: (integerp 3.14) => NIL"
  (ok (not (clysm/tests:compile-and-run '(integerp 3.14)))
      "(integerp 3.14) should be NIL"))

;;; rationalp tests

(deftest test-rationalp-fixnum
  "rationalp on fixnum: (rationalp 42) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 42))
      "(rationalp 42) should be T"))

(deftest test-rationalp-bignum
  "rationalp on bignum: (rationalp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 10000000000000))
      "(rationalp 10000000000000) should be T"))

(deftest test-rationalp-ratio
  "rationalp on ratio: (rationalp 1/2) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 1/2))
      "(rationalp 1/2) should be T"))

(deftest test-rationalp-float
  "rationalp on float: (rationalp 3.14) => NIL"
  (ok (not (clysm/tests:compile-and-run '(rationalp 3.14)))
      "(rationalp 3.14) should be NIL"))

;;; realp tests

(deftest test-realp-fixnum
  "realp on fixnum: (realp 42) => T"
  (ok (clysm/tests:compile-and-run '(realp 42))
      "(realp 42) should be T"))

(deftest test-realp-float
  "realp on float: (realp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(realp 3.14))
      "(realp 3.14) should be T"))

(deftest test-realp-complex
  "realp on complex: (realp #C(1 2)) => NIL"
  (ok (not (clysm/tests:compile-and-run '(realp #C(1 2))))
      "(realp #C(1 2)) should be NIL"))

(deftest test-realp-complex-zero-imag
  "realp on complex with zero imag: (realp #C(5 0)) => T"
  ;; #C(5 0) simplifies to 5 in Common Lisp
  (ok (clysm/tests:compile-and-run '(realp #C(5 0)))
      "(realp #C(5 0)) should be T (simplifies to 5)"))

;;; floatp tests

(deftest test-floatp-float
  "floatp on float: (floatp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(floatp 3.14))
      "(floatp 3.14) should be T"))

(deftest test-floatp-integer
  "floatp on integer: (floatp 42) => NIL"
  (ok (not (clysm/tests:compile-and-run '(floatp 42)))
      "(floatp 42) should be NIL"))

;;; complexp tests

(deftest test-complexp-complex
  "complexp on complex: (complexp #C(1 2)) => T"
  (ok (clysm/tests:compile-and-run '(complexp #C(1 2)))
      "(complexp #C(1 2)) should be T"))

(deftest test-complexp-integer
  "complexp on integer: (complexp 42) => NIL"
  (ok (not (clysm/tests:compile-and-run '(complexp 42)))
      "(complexp 42) should be NIL"))

(deftest test-complexp-complex-zero-imag
  "complexp on complex with zero imag: (complexp #C(5 0)) => NIL"
  ;; #C(5 0) simplifies to 5 in Common Lisp
  (ok (not (clysm/tests:compile-and-run '(complexp #C(5 0))))
      "(complexp #C(5 0)) should be NIL (simplifies to 5)"))

;;; zerop tests

(deftest test-zerop-zero
  "zerop on zero: (zerop 0) => T"
  (ok (clysm/tests:compile-and-run '(zerop 0))
      "(zerop 0) should be T"))

(deftest test-zerop-nonzero
  "zerop on nonzero: (zerop 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(zerop 5)))
      "(zerop 5) should be NIL"))

(deftest test-zerop-float-zero
  "zerop on float zero: (zerop 0.0) => T"
  (ok (clysm/tests:compile-and-run '(zerop 0.0))
      "(zerop 0.0) should be T"))

(deftest test-zerop-complex-zero
  "zerop on complex zero: (zerop #C(0 0)) => T"
  (ok (clysm/tests:compile-and-run '(zerop #C(0 0)))
      "(zerop #C(0 0)) should be T"))

;;; plusp tests

(deftest test-plusp-positive
  "plusp on positive: (plusp 5) => T"
  (ok (clysm/tests:compile-and-run '(plusp 5))
      "(plusp 5) should be T"))

(deftest test-plusp-zero
  "plusp on zero: (plusp 0) => NIL"
  (ok (not (clysm/tests:compile-and-run '(plusp 0)))
      "(plusp 0) should be NIL"))

(deftest test-plusp-negative
  "plusp on negative: (plusp -5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(plusp -5)))
      "(plusp -5) should be NIL"))

;;; minusp tests

(deftest test-minusp-negative
  "minusp on negative: (minusp -5) => T"
  (ok (clysm/tests:compile-and-run '(minusp -5))
      "(minusp -5) should be T"))

(deftest test-minusp-zero
  "minusp on zero: (minusp 0) => NIL"
  (ok (not (clysm/tests:compile-and-run '(minusp 0)))
      "(minusp 0) should be NIL"))

(deftest test-minusp-positive
  "minusp on positive: (minusp 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(minusp 5)))
      "(minusp 5) should be NIL"))

;;; evenp tests

(deftest test-evenp-even
  "evenp on even: (evenp 4) => T"
  (ok (clysm/tests:compile-and-run '(evenp 4))
      "(evenp 4) should be T"))

(deftest test-evenp-odd
  "evenp on odd: (evenp 3) => NIL"
  (ok (not (clysm/tests:compile-and-run '(evenp 3)))
      "(evenp 3) should be NIL"))

(deftest test-evenp-zero
  "evenp on zero: (evenp 0) => T"
  (ok (clysm/tests:compile-and-run '(evenp 0))
      "(evenp 0) should be T"))

(deftest test-evenp-negative-even
  "evenp on negative even: (evenp -4) => T"
  (ok (clysm/tests:compile-and-run '(evenp -4))
      "(evenp -4) should be T"))

(deftest test-evenp-bignum
  "evenp on bignum: (evenp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(evenp 10000000000000))
      "(evenp 10000000000000) should be T"))

;;; oddp tests

(deftest test-oddp-odd
  "oddp on odd: (oddp 3) => T"
  (ok (clysm/tests:compile-and-run '(oddp 3))
      "(oddp 3) should be T"))

(deftest test-oddp-even
  "oddp on even: (oddp 4) => NIL"
  (ok (not (clysm/tests:compile-and-run '(oddp 4)))
      "(oddp 4) should be NIL"))

(deftest test-oddp-negative-odd
  "oddp on negative odd: (oddp -3) => T"
  (ok (clysm/tests:compile-and-run '(oddp -3))
      "(oddp -3) should be T"))

;;; ============================================================
;;; Phase 14B: Bit Testing Functions (US3)
;;; logbitp - test if bit at index is set
;;; logtest - test if two integers share any set bits
;;; ============================================================

;;; logbitp tests

(deftest test-logbitp-bit0-set
  "logbitp on bit 0 of 5 (101 binary): (logbitp 0 5) => T"
  (ok (clysm/tests:compile-and-run '(logbitp 0 5))
      "(logbitp 0 5) should be T"))

(deftest test-logbitp-bit1-unset
  "logbitp on bit 1 of 5 (101 binary): (logbitp 1 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(logbitp 1 5)))
      "(logbitp 1 5) should be NIL"))

(deftest test-logbitp-bit2-set
  "logbitp on bit 2 of 5 (101 binary): (logbitp 2 5) => T"
  (ok (clysm/tests:compile-and-run '(logbitp 2 5))
      "(logbitp 2 5) should be T"))

(deftest test-logbitp-high-bit-unset
  "logbitp on bit 10 of 5: (logbitp 10 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(logbitp 10 5)))
      "(logbitp 10 5) should be NIL"))

(deftest test-logbitp-negative-integer
  "logbitp on negative integer: (logbitp 0 -1) => T (all bits set)"
  (ok (clysm/tests:compile-and-run '(logbitp 0 -1))
      "(logbitp 0 -1) should be T"))

;;; logtest tests

(deftest test-logtest-common-bits
  "logtest with common bits: (logtest 5 3) => T (both have bit 0)"
  (ok (clysm/tests:compile-and-run '(logtest 5 3))
      "(logtest 5 3) should be T"))

(deftest test-logtest-no-common-bits
  "logtest with no common bits: (logtest 4 3) => NIL"
  (ok (not (clysm/tests:compile-and-run '(logtest 4 3)))
      "(logtest 4 3) should be NIL"))

(deftest test-logtest-same-value
  "logtest with same value: (logtest 7 7) => T"
  (ok (clysm/tests:compile-and-run '(logtest 7 7))
      "(logtest 7 7) should be T"))

(deftest test-logtest-zero
  "logtest with zero: (logtest 0 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(logtest 0 5)))
      "(logtest 0 5) should be NIL"))

;;; ============================================================
;;; Phase 14B: Byte Specifier Functions (US4)
;;; byte - create byte specifier
;;; byte-size - extract size from byte specifier
;;; byte-position - extract position from byte specifier
;;; ============================================================

;;; byte constructor tests

(deftest test-byte-constructor
  "byte constructor: (byte 8 0) creates a byte specifier"
  ;; byte specifier encoding: (size << 6) | position = (8 << 6) | 0 = 512
  (ok (clysm/tests:compile-and-run '(integerp (byte 8 0)))
      "(byte 8 0) should return an integer (byte specifier)"))

(deftest test-byte-size-basic
  "byte-size extracts size: (byte-size (byte 8 4)) => 8"
  (ok (= 8 (clysm/tests:compile-and-run '(byte-size (byte 8 4))))
      "(byte-size (byte 8 4)) should be 8"))

(deftest test-byte-position-basic
  "byte-position extracts position: (byte-position (byte 8 4)) => 4"
  (ok (= 4 (clysm/tests:compile-and-run '(byte-position (byte 8 4))))
      "(byte-position (byte 8 4)) should be 4"))

(deftest test-byte-size-zero
  "byte-size with zero size: (byte-size (byte 0 4)) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(byte-size (byte 0 4))))
      "(byte-size (byte 0 4)) should be 0"))

(deftest test-byte-position-zero
  "byte-position with zero position: (byte-position (byte 8 0)) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(byte-position (byte 8 0))))
      "(byte-position (byte 8 0)) should be 0"))

;;; ============================================================
;;; Phase 14B: Byte Extraction and Manipulation (US5)
;;; ldb - load byte (extract and shift)
;;; dpb - deposit byte (replace field)
;;; mask-field - extract keeping position
;;; deposit-field - deposit pre-positioned field
;;; ============================================================

;;; ldb tests

(deftest test-ldb-high-nibble
  "ldb extracts high nibble: (ldb (byte 4 4) #xAB) => #xA"
  (ok (= #xA (clysm/tests:compile-and-run '(ldb (byte 4 4) #xAB)))
      "(ldb (byte 4 4) #xAB) should be #xA"))

(deftest test-ldb-low-nibble
  "ldb extracts low nibble: (ldb (byte 4 0) #xAB) => #xB"
  (ok (= #xB (clysm/tests:compile-and-run '(ldb (byte 4 0) #xAB)))
      "(ldb (byte 4 0) #xAB) should be #xB"))

(deftest test-ldb-empty-byte
  "ldb with empty byte: (ldb (byte 0 4) #xAB) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(ldb (byte 0 4) #xAB)))
      "(ldb (byte 0 4) #xAB) should be 0"))

(deftest test-ldb-full-byte
  "ldb extracts full byte: (ldb (byte 8 0) #xAB) => #xAB"
  (ok (= #xAB (clysm/tests:compile-and-run '(ldb (byte 8 0) #xAB)))
      "(ldb (byte 8 0) #xAB) should be #xAB"))

;;; dpb tests

(deftest test-dpb-replace-high-nibble
  "dpb replaces high nibble: (dpb #xC (byte 4 4) #xAB) => #xCB"
  (ok (= #xCB (clysm/tests:compile-and-run '(dpb #xC (byte 4 4) #xAB)))
      "(dpb #xC (byte 4 4) #xAB) should be #xCB"))

(deftest test-dpb-replace-low-nibble
  "dpb replaces low nibble: (dpb #xD (byte 4 0) #xAB) => #xAD"
  (ok (= #xAD (clysm/tests:compile-and-run '(dpb #xD (byte 4 0) #xAB)))
      "(dpb #xD (byte 4 0) #xAB) should be #xAD"))

(deftest test-dpb-empty-byte
  "dpb with empty byte: (dpb #xF (byte 0 4) #xAB) => #xAB (unchanged)"
  (ok (= #xAB (clysm/tests:compile-and-run '(dpb #xF (byte 0 4) #xAB)))
      "(dpb #xF (byte 0 4) #xAB) should be #xAB"))

;;; mask-field tests

(deftest test-mask-field-high-nibble
  "mask-field extracts in place: (mask-field (byte 4 4) #xAB) => #xA0"
  (ok (= #xA0 (clysm/tests:compile-and-run '(mask-field (byte 4 4) #xAB)))
      "(mask-field (byte 4 4) #xAB) should be #xA0"))

(deftest test-mask-field-low-nibble
  "mask-field on low nibble: (mask-field (byte 4 0) #xAB) => #x0B"
  (ok (= #x0B (clysm/tests:compile-and-run '(mask-field (byte 4 0) #xAB)))
      "(mask-field (byte 4 0) #xAB) should be #x0B"))

(deftest test-mask-field-empty-byte
  "mask-field with empty byte: (mask-field (byte 0 4) #xAB) => 0"
  (ok (= 0 (clysm/tests:compile-and-run '(mask-field (byte 0 4) #xAB)))
      "(mask-field (byte 0 4) #xAB) should be 0"))

;;; deposit-field tests

(deftest test-deposit-field-high-nibble
  "deposit-field with pre-positioned value: (deposit-field #xC0 (byte 4 4) #xAB) => #xCB"
  (ok (= #xCB (clysm/tests:compile-and-run '(deposit-field #xC0 (byte 4 4) #xAB)))
      "(deposit-field #xC0 (byte 4 4) #xAB) should be #xCB"))

(deftest test-deposit-field-low-nibble
  "deposit-field on low nibble: (deposit-field #x0D (byte 4 0) #xAB) => #xAD"
  (ok (= #xAD (clysm/tests:compile-and-run '(deposit-field #x0D (byte 4 0) #xAB)))
      "(deposit-field #x0D (byte 4 0) #xAB) should be #xAD"))

(deftest test-deposit-field-empty-byte
  "deposit-field with empty byte: (deposit-field #xF0 (byte 0 4) #xAB) => #xAB"
  (ok (= #xAB (clysm/tests:compile-and-run '(deposit-field #xF0 (byte 0 4) #xAB)))
      "(deposit-field #xF0 (byte 0 4) #xAB) should be #xAB"))
