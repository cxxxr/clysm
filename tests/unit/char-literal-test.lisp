;;;; char-literal-test.lisp - Unit tests for character literal compilation
;;;; Feature: 001-char-literal-compile (Phase 13D-1a)
;;;;
;;;; Tests compile-quoted-element with character literals.
;;;; Per Constitution Principle VII (TDD), these tests are written FIRST
;;;; and must FAIL before implementation.

(in-package #:clysm/tests/unit/char-literal)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun compile-quoted-element (elem)
  "Compile a quoted element using the compiler's internal function."
  (clysm/compiler/codegen/func-section::compile-quoted-element elem))

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Character Literals (P1)
;;; Tests for whitespace character literals (#\Space, #\Tab, #\Newline)
;;; ============================================================

(deftest test-compile-space-character
  "T005: Verify #\\Space compiles to i31ref with code 32"
  (let ((result (compile-quoted-element #\Space)))
    (ok (equal result '((:i32.const 32) :ref.i31))
        "T005: #\\Space should compile to ((:i32.const 32) :ref.i31)")))

(deftest test-compile-tab-character
  "T006: Verify #\\Tab compiles to i31ref with code 9"
  (let ((result (compile-quoted-element #\Tab)))
    (ok (equal result '((:i32.const 9) :ref.i31))
        "T006: #\\Tab should compile to ((:i32.const 9) :ref.i31)")))

(deftest test-compile-newline-character
  "T007: Verify #\\Newline compiles to i31ref with code 10"
  (let ((result (compile-quoted-element #\Newline)))
    (ok (equal result '((:i32.const 10) :ref.i31))
        "T007: #\\Newline should compile to ((:i32.const 10) :ref.i31)")))

;;; ============================================================
;;; Phase 4: User Story 2 - Extended Character Literals (P2)
;;; Tests for printable chars and additional named characters
;;; ============================================================

(deftest test-compile-lowercase-a-character
  "T014: Verify #\\a compiles to i31ref with code 97"
  (let ((result (compile-quoted-element #\a)))
    (ok (equal result '((:i32.const 97) :ref.i31))
        "T014: #\\a should compile to ((:i32.const 97) :ref.i31)")))

(deftest test-compile-uppercase-a-character
  "T015: Verify #\\A compiles to i31ref with code 65"
  (let ((result (compile-quoted-element #\A)))
    (ok (equal result '((:i32.const 65) :ref.i31))
        "T015: #\\A should compile to ((:i32.const 65) :ref.i31)")))

(deftest test-compile-return-character
  "T016: Verify #\\Return compiles to i31ref with code 13"
  (let ((result (compile-quoted-element #\Return)))
    (ok (equal result '((:i32.const 13) :ref.i31))
        "T016: #\\Return should compile to ((:i32.const 13) :ref.i31)")))
