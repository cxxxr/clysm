;;;; write-to-string-test.lisp - Unit tests for write-to-string function
;;;; Feature: 001-numeric-format (Phase 14C)

(in-package #:clysm/tests/unit/numeric/write-to-string)

;;; ============================================================
;;; User Story 2: Integer Base Conversion Output
;;; ============================================================

;;; T018: write-to-string tests - TDD (write first, must fail until implemented)

(deftest write-to-string-hex-42
  (testing "write-to-string 42 :base 16 should return 2A"
    (let ((result (compile-and-run '(write-to-string 42 :base 16))))
      (ok (string= result "2A")
          "write-to-string 42 :base 16 should produce 2A"))))

(deftest write-to-string-hex-255
  (testing "write-to-string 255 :base 16 should return FF"
    (let ((result (compile-and-run '(write-to-string 255 :base 16))))
      (ok (string= result "FF")
          "write-to-string 255 :base 16 should produce FF"))))

(deftest write-to-string-binary-42
  (testing "write-to-string 42 :base 2 should return 101010"
    (let ((result (compile-and-run '(write-to-string 42 :base 2))))
      (ok (string= result "101010")
          "write-to-string 42 :base 2 should produce 101010"))))

(deftest write-to-string-octal-42
  (testing "write-to-string 42 :base 8 should return 52"
    (let ((result (compile-and-run '(write-to-string 42 :base 8))))
      (ok (string= result "52")
          "write-to-string 42 :base 8 should produce 52"))))

(deftest write-to-string-negative-hex
  (testing "write-to-string -42 :base 16 should return -2A"
    (let ((result (compile-and-run '(write-to-string -42 :base 16))))
      (ok (string= result "-2A")
          "write-to-string -42 :base 16 should produce -2A"))))

;;; ============================================================
;;; User Story 3: Standard Number String Output
;;; ============================================================

(deftest write-to-string-integer-default
  (testing "write-to-string 42 should return 42 in base 10"
    (let ((result (compile-and-run '(write-to-string 42))))
      (ok (string= result "42")
          "write-to-string 42 should produce 42"))))

(deftest write-to-string-zero
  (testing "write-to-string 0 should return 0"
    (let ((result (compile-and-run '(write-to-string 0))))
      (ok (string= result "0")
          "write-to-string 0 should produce 0"))))

(deftest write-to-string-large-number
  (testing "write-to-string 12345 should return 12345"
    (let ((result (compile-and-run '(write-to-string 12345))))
      (ok (string= result "12345")
          "write-to-string 12345 should produce 12345"))))

;;; Note: Ratio and float tests use placeholder strings in MVP
(deftest write-to-string-ratio-placeholder
  (testing "write-to-string 1/2 returns placeholder for MVP"
    (let ((result (compile-and-run '(write-to-string 1/2))))
      (ok (stringp result)
          "write-to-string ratio should return a string"))))

(deftest write-to-string-float-placeholder
  (testing "write-to-string 3.14 returns placeholder for MVP"
    (let ((result (compile-and-run '(write-to-string 3.14))))
      (ok (stringp result)
          "write-to-string float should return a string"))))
