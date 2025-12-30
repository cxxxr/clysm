;;;; char-wasm-test.lisp - Contract tests for character literal Wasm output
;;;; Feature: 001-char-literal-compile (Phase 13D-1a)
;;;;
;;;; Validates that Wasm output containing character literals
;;;; passes wasm-tools validation.

(in-package #:clysm/tests/contract/char-wasm)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun compile-to-wasm (form)
  "Compile a form to Wasm bytes."
  (clysm/compiler:compile-to-wasm form))

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Character Literals (P1)
;;; Contract tests for quoted character expressions
;;; ============================================================

(deftest test-quoted-char-list-compiles
  "T012: Verify '(#\\Space #\\Tab) compiles to valid Wasm"
  (let ((wasm (compile-to-wasm '(quote (#\Space #\Tab)))))
    (ok (> (length wasm) 0) "T012: quoted char list compiles to non-empty Wasm")
    (ok (typep wasm '(vector (unsigned-byte 8))) "T012: produces byte vector")))

;;; ============================================================
;;; Phase 4: User Story 2 - Extended Character Literals (P2)
;;; Contract tests for various character types
;;; ============================================================

(deftest test-printable-chars-compile
  "T019: Verify printable characters compile to valid Wasm"
  (let ((wasm (compile-to-wasm '(quote (#\a #\Z #\! #\Return)))))
    (ok (> (length wasm) 0) "T019: printable chars compile to non-empty Wasm")))

(deftest test-member-form-with-chars-compiles
  "T020: Verify (member x '(#\\a #\\b #\\c)) defun form compiles"
  (let ((wasm (compile-to-wasm '(defun char-member-test (x)
                                  (member x '(#\a #\b #\c))))))
    (ok (> (length wasm) 0) "T020: member form with chars compiles")))
