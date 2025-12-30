;;;; numeric-format-test.lisp - Integration tests for numeric conversion/formatting
;;;; Feature: 001-numeric-format (Phase 14C)

(in-package #:clysm/tests/integration/numeric-format)

;;; ============================================================
;;; T034: End-to-end compilation tests for rationalize and write-to-string
;;; ============================================================

;;; --- Rationalize Tests ---

(deftest rationalize-compiles-and-validates
  "rationalize expressions compile to valid Wasm"
  (testing "rationalize 0.5"
    (ok (validate-wasm-silent (clysm:compile-to-wasm '(rationalize 0.5)))
        "(rationalize 0.5) should produce valid Wasm"))
  (testing "rationalize integer passthrough"
    (ok (validate-wasm-silent (clysm:compile-to-wasm '(rationalize 5)))
        "(rationalize 5) should produce valid Wasm"))
  (testing "rationalize ratio passthrough"
    (ok (validate-wasm-silent (clysm:compile-to-wasm '(rationalize 1/2)))
        "(rationalize 1/2) should produce valid Wasm")))

(deftest rationalize-float-to-ratio
  "rationalize converts floats to ratios correctly"
  (testing "0.5 -> 1/2"
    (let ((result (compile-and-run '(rationalize 0.5))))
      (ok (rationalp result)
          "(rationalize 0.5) should return a rational")
      (ok (= 1/2 result)
          "(rationalize 0.5) should equal 1/2")))
  (testing "3.0 -> 3"
    (let ((result (compile-and-run '(rationalize 3.0))))
      (ok (integerp result)
          "(rationalize 3.0) should return an integer")
      (ok (= 3 result)
          "(rationalize 3.0) should equal 3"))))

(deftest rationalize-passthrough
  "rationalize passes through integers and ratios unchanged"
  (testing "integer passthrough"
    (let ((result (compile-and-run '(rationalize 42))))
      (ok (= 42 result)
          "(rationalize 42) should return 42")))
  (testing "ratio passthrough"
    (let ((result (compile-and-run '(rationalize 3/4))))
      (ok (= 3/4 result)
          "(rationalize 3/4) should return 3/4"))))

;;; --- Write-to-String Tests ---

(deftest write-to-string-compiles-and-validates
  "write-to-string expressions compile to valid Wasm"
  (testing "write-to-string integer"
    (ok (validate-wasm-silent (clysm:compile-to-wasm '(write-to-string 42)))
        "(write-to-string 42) should produce valid Wasm"))
  (testing "write-to-string with base"
    (ok (validate-wasm-silent (clysm:compile-to-wasm '(write-to-string 42 :base 16)))
        "(write-to-string 42 :base 16) should produce valid Wasm")))

(deftest write-to-string-decimal
  "write-to-string converts integers to decimal strings"
  (testing "42 -> \"42\""
    (let ((result (compile-and-run '(write-to-string 42))))
      (ok (stringp result)
          "(write-to-string 42) should return a string")
      (ok (string= "42" result)
          "(write-to-string 42) should return \"42\"")))
  (testing "0 -> \"0\""
    (let ((result (compile-and-run '(write-to-string 0))))
      (ok (string= "0" result)
          "(write-to-string 0) should return \"0\""))))

(deftest write-to-string-hexadecimal
  "write-to-string converts integers to hexadecimal strings"
  (testing "42 base 16 -> \"2A\""
    (let ((result (compile-and-run '(write-to-string 42 :base 16))))
      (ok (string= "2A" result)
          "(write-to-string 42 :base 16) should return \"2A\"")))
  (testing "255 base 16 -> \"FF\""
    (let ((result (compile-and-run '(write-to-string 255 :base 16))))
      (ok (string= "FF" result)
          "(write-to-string 255 :base 16) should return \"FF\""))))

(deftest write-to-string-binary
  "write-to-string converts integers to binary strings"
  (testing "42 base 2 -> \"101010\""
    (let ((result (compile-and-run '(write-to-string 42 :base 2))))
      (ok (string= "101010" result)
          "(write-to-string 42 :base 2) should return \"101010\""))))

(deftest write-to-string-negative
  "write-to-string handles negative integers"
  (testing "-42 base 16 -> \"-2A\""
    (let ((result (compile-and-run '(write-to-string -42 :base 16))))
      (ok (string= "-2A" result)
          "(write-to-string -42 :base 16) should return \"-2A\""))))

;;; --- Combined Tests ---

(deftest rationalize-write-to-string-combined
  "rationalize and write-to-string work together"
  (testing "rationalize then write-to-string"
    (let ((result (compile-and-run '(let ((r (rationalize 0.5)))
                                      (write-to-string (numerator r))))))
      (ok (stringp result)
          "Combined expression should return a string"))))
