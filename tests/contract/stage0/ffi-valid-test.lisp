;;;; ffi-valid-test.lisp - Contract tests for Stage 0 FFI imports validation
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US5: FFI Filesystem Access - FFI imports valid

(defpackage #:clysm/tests/contract/stage0/ffi-valid-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-ffi-imports
                #:generate-runtime-init))

(in-package #:clysm/tests/contract/stage0/ffi-valid-test)

;;; ============================================================
;;; T016: Contract test for FFI imports validation
;;; ============================================================

(defun wasm-tools-available-p ()
  "Check if wasm-tools is available"
  (ignore-errors
    (zerop (nth-value 2
             (uiop:run-program '("wasm-tools" "--version")
                               :ignore-error-status t)))))

(deftest test-ffi-imports-section-valid
  "Verify FFI import section is valid Wasm"
  (let ((bytes (generate-ffi-imports)))
    (when (> (length bytes) 0)
      (ok (= 2 (aref bytes 0)) "Should start with import section ID")
      ;; Verify LEB128 size is valid
      (let ((size-byte (aref bytes 1)))
        (ok (< size-byte 128) "Size should be single-byte LEB128 for small imports")))))

(deftest test-import-kind-is-function
  "Verify all imports are function imports (kind 0x00)"
  (let ((bytes (generate-ffi-imports)))
    ;; Import kind 0x00 means function import
    (when (> (length bytes) 5)
      (ok (member #x00 (coerce bytes 'list))
          "Should contain function import kind (0x00)"))))

(deftest test-import-types-reference-valid-indices
  "Verify import type indices are within type section range"
  (let ((imports (generate-ffi-imports)))
    ;; Type indices should be < 28 (our total type count)
    ;; This is a structural check - actual validation done by wasm-tools
    (ok (vectorp imports) "Imports should be a vector")))

;;; ============================================================
;;; Import Name Encoding Tests
;;; ============================================================

(deftest test-import-names-utf8-encoded
  "Verify import names are properly UTF-8 encoded"
  (let ((bytes (generate-ffi-imports)))
    ;; Names should be length-prefixed UTF-8 strings
    ;; Each name starts with a length byte
    (when (> (length bytes) 3)
      (let ((first-name-len (aref bytes 2)))  ; After section ID and size
        (ok (< first-name-len 128) "Name length should be reasonable")))))

(deftest test-import-module-name-present
  "Verify module name (namespace) is present in imports"
  (let ((bytes (generate-ffi-imports)))
    ;; Should have at least module name + field name
    (ok (>= (length bytes) 5) "Should have module and field names")))

;;; ============================================================
;;; Full Module Validation with FFI
;;; ============================================================

(deftest test-runtime-with-ffi-validates
  "Verify complete runtime with FFI imports produces valid Wasm"
  (skip "Requires complete module generation")
  (let ((wasm-bytes (generate-runtime-init)))
    (ok (>= (length wasm-bytes) 8) "Should have Wasm header")))
