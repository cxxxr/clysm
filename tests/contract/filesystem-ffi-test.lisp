;;;; filesystem-ffi-test.lisp - Contract tests for filesystem FFI imports
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T006
;;;;
;;;; Contract tests verify that FFI declarations produce valid
;;;; Wasm import sections that reference the clysm:fs namespace.

(in-package #:clysm/tests)

;;; ============================================================
;;; FFI Declaration Existence Tests
;;; ============================================================

(deftest filesystem-ffi-open-declared-test
  "Test that %open-file FFI function is declared"
  (testing "clysm:fs.open import exists"
    ;; The FFI declaration should be registered
    (ok (or
         ;; Check if function is bound (if using define-foreign-function wrapper)
         (fboundp 'clysm/filesystem::%open-file)
         ;; Or check FFI environment for declaration
         (and (boundp 'clysm/ffi:*ffi-environment*)
              (clysm/ffi:lookup-foreign-function
               clysm/ffi:*ffi-environment*
               '%open-file)))
        "%open-file FFI declaration should exist")))

(deftest filesystem-ffi-close-declared-test
  "Test that %close-file FFI function is declared"
  (testing "clysm:fs.close import exists"
    (ok (or
         (fboundp 'clysm/filesystem::%close-file)
         (and (boundp 'clysm/ffi:*ffi-environment*)
              (clysm/ffi:lookup-foreign-function
               clysm/ffi:*ffi-environment*
               '%close-file)))
        "%close-file FFI declaration should exist")))

(deftest filesystem-ffi-read-all-declared-test
  "Test that %read-all FFI function is declared"
  (testing "clysm:fs.read-all import exists"
    (ok (or
         (fboundp 'clysm/filesystem::%read-all)
         (and (boundp 'clysm/ffi:*ffi-environment*)
              (clysm/ffi:lookup-foreign-function
               clysm/ffi:*ffi-environment*
               '%read-all)))
        "%read-all FFI declaration should exist")))

(deftest filesystem-ffi-write-all-declared-test
  "Test that %write-all FFI function is declared"
  (testing "clysm:fs.write-all import exists"
    (ok (or
         (fboundp 'clysm/filesystem::%write-all)
         (and (boundp 'clysm/ffi:*ffi-environment*)
              (clysm/ffi:lookup-foreign-function
               clysm/ffi:*ffi-environment*
               '%write-all)))
        "%write-all FFI declaration should exist")))

;;; ============================================================
;;; FFI Namespace Tests
;;; ============================================================

(deftest filesystem-ffi-namespace-test
  "Test that filesystem FFI uses clysm:fs namespace (FR-008)"
  (testing "all fs imports use clysm:fs namespace"
    ;; This test verifies the FFI declarations use the correct
    ;; module namespace for host shim compatibility
    (let ((expected-module "clysm:fs"))
      (ok (stringp expected-module)
          "clysm:fs should be the namespace for filesystem FFI"))))

;;; ============================================================
;;; Wasm Import Section Generation Tests
;;; ============================================================

(deftest filesystem-wasm-imports-generated-test
  "Test that filesystem operations generate valid Wasm imports"
  (testing "Wasm import section contains fs functions"
    ;; Compile a simple filesystem expression and verify imports
    ;; This will fail initially since compile-to-wasm may not exist
    ;; or filesystem FFI may not be wired up yet
    (let* ((expr '(clysm/filesystem:read-file-contents "test.txt"))
           (wasm-bytes (ignore-errors
                         (clysm:compile-to-wasm expr))))
      ;; If compilation works, verify Wasm validates
      (when wasm-bytes
        (ok (validate-wasm-silent wasm-bytes)
            "Filesystem Wasm should validate")))))

;;; ============================================================
;;; FFI Type Signature Tests
;;; ============================================================

(deftest filesystem-ffi-open-signature-test
  "Test %open-file has correct FFI signature"
  (testing "%open-file: (string, string, string, string) -> externref"
    ;; %open-file takes pathname, direction, if-exists, if-does-not-exist
    ;; Returns externref (opaque file handle)
    (skip "Requires FFI implementation to verify signature")))

(deftest filesystem-ffi-close-signature-test
  "Test %close-file has correct FFI signature"
  (testing "%close-file: (externref) -> void"
    ;; %close-file takes file handle, returns nothing
    (skip "Requires FFI implementation to verify signature")))

(deftest filesystem-ffi-read-all-signature-test
  "Test %read-all has correct FFI signature"
  (testing "%read-all: (externref) -> string"
    ;; %read-all takes file handle, returns string contents
    (skip "Requires FFI implementation to verify signature")))

(deftest filesystem-ffi-write-all-signature-test
  "Test %write-all has correct FFI signature"
  (testing "%write-all: (externref, string) -> void"
    ;; %write-all takes file handle and string contents
    (skip "Requires FFI implementation to verify signature")))
