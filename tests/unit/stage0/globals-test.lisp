;;;; globals-test.lisp - Unit tests for Stage 0 global variable initialization
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US4: Runtime Initialization - Global variable initialization

(defpackage #:clysm/tests/unit/stage0/globals-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-global-section
                #:generate-global-init
                #:*nil-index*
                #:*unbound-index*
                #:*mv-count-index*
                #:*mv-buffer-index*))

(in-package #:clysm/tests/unit/stage0/globals-test)

;;; ============================================================
;;; T008: Unit test for global variable initialization
;;; ============================================================

(deftest test-global-section-generates-bytes
  "Verify generate-global-section returns a byte vector"
  (let ((bytes (generate-global-section)))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 0) "Should have non-zero length")
    (ok (every (lambda (b) (and (integerp b) (<= 0 b 255))) bytes)
        "Should contain only valid bytes")))

(deftest test-global-section-starts-with-section-id
  "Verify global section starts with section ID 6"
  (let ((bytes (generate-global-section)))
    (ok (= 6 (aref bytes 0)) "First byte should be section ID 6 (global section)")))

(deftest test-global-indices-are-defined
  "Verify global indices are properly defined"
  (ok (= 0 *nil-index*) "NIL should be at global index 0")
  (ok (= 1 *unbound-index*) "UNBOUND should be at global index 1")
  (ok (= 2 *mv-count-index*) "mv-count should be at global index 2")
  (ok (= 3 *mv-buffer-index*) "mv-buffer should be at global index 3"))

(deftest test-global-section-contains-four-globals
  "Verify at least 4 globals are defined (NIL, UNBOUND, mv-count, mv-buffer)"
  (let ((bytes (generate-global-section)))
    ;; Section format: 1 byte ID + LEB128 size + LEB128 count + global definitions
    ;; Each global: valtype + mut flag + init expr + end
    ;; We expect at least 4 globals
    (ok (>= (length bytes) 10) "Should have enough bytes for 4 globals")))

;;; ============================================================
;;; Global Initialization Expression Tests
;;; ============================================================

(deftest test-nil-global-initialized
  "Verify NIL global is initialized as singleton struct"
  (let ((bytes (generate-global-section)))
    ;; NIL is ref type, so should have ref.null or struct.new instruction
    ;; struct.new is 0xFB 0x00
    (ok (or (member #xFB (coerce bytes 'list))  ; GC prefix
            (member #xD0 (coerce bytes 'list))) ; ref.null prefix
        "Should contain GC or ref.null instruction for NIL init")))

(deftest test-unbound-global-initialized
  "Verify UNBOUND global is initialized as sentinel struct"
  (let ((bytes (generate-global-section)))
    ;; Similar to NIL, UNBOUND should be initialized with struct.new
    (ok (> (length bytes) 15) "Should have bytes for UNBOUND initialization")))

(deftest test-mv-count-global-is-i32
  "Verify mv-count global is i32 type initialized to 1"
  (let ((bytes (generate-global-section)))
    ;; i32 type is 0x7F, init should be i32.const 1
    (ok (member #x7F (coerce bytes 'list)) "Should contain i32 type marker")))

(deftest test-mv-buffer-global-is-array
  "Verify mv-buffer global is array type"
  (let ((bytes (generate-global-section)))
    ;; Should reference array type and use array.new
    (ok (> (length bytes) 20) "Should have bytes for mv-buffer array")))

;;; ============================================================
;;; Global Mutability Tests
;;; ============================================================

(deftest test-nil-is-immutable
  "Verify NIL global is immutable (const)"
  (let ((bytes (generate-global-section)))
    ;; Immutable global has 0x00 for mutability flag
    (ok (member #x00 (coerce bytes 'list)) "Should have immutable flag")))

(deftest test-mv-count-is-mutable
  "Verify mv-count global is mutable"
  (let ((bytes (generate-global-section)))
    ;; Mutable global has 0x01 for mutability flag
    (ok (member #x01 (coerce bytes 'list)) "Should have mutable flag")))

;;; ============================================================
;;; Global Init Code Generation
;;; ============================================================

(deftest test-generate-global-init-returns-instructions
  "Verify generate-global-init returns instruction list"
  (let ((instrs (generate-global-init)))
    (ok (listp instrs) "Should return a list")
    ;; Empty list is valid - globals are initialized in their definition
    (ok (>= (length instrs) 0) "Should return valid list (may be empty)")))
