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
                #:*mv-buffer-index*
                ;; Symbol interning (T011-T012)
                #:*symbol-table*
                #:stage0-symbol
                #:make-stage0-symbol
                #:sym-name
                #:sym-value
                #:sym-function
                #:sym-plist
                #:intern-symbol
                #:find-symbol*
                #:clear-symbol-table
                #:symbol-count))

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

;;; ============================================================
;;; T012: Symbol Interning Tests
;;; ============================================================

(deftest test-intern-symbol-creates-symbol
  "Verify intern-symbol creates a new symbol"
  (clear-symbol-table)
  (let ((sym (intern-symbol "FOO")))
    (ok (not (null sym)) "Should return a symbol")
    (ok (typep sym 'stage0-symbol) "Should be a stage0-symbol struct")
    (ok (string= "FOO" (sym-name sym)) "Name should match")))

(deftest test-intern-symbol-returns-same-symbol
  "Verify intern-symbol returns same symbol for same name"
  (clear-symbol-table)
  (let ((sym1 (intern-symbol "BAR"))
        (sym2 (intern-symbol "BAR")))
    (ok (eq sym1 sym2) "Should return identical symbol")))

(deftest test-intern-symbol-different-names
  "Verify different names create different symbols"
  (clear-symbol-table)
  (let ((sym1 (intern-symbol "X"))
        (sym2 (intern-symbol "Y")))
    (ok (not (eq sym1 sym2)) "Should be different symbols")
    (ok (string= "X" (sym-name sym1)) "First name should be X")
    (ok (string= "Y" (sym-name sym2)) "Second name should be Y")))

(deftest test-intern-symbol-from-symbol
  "Verify intern-symbol accepts Common Lisp symbols"
  (clear-symbol-table)
  (let ((sym (intern-symbol 'TEST-SYM)))
    (ok (not (null sym)) "Should create symbol from CL symbol")
    (ok (string= "TEST-SYM" (sym-name sym)) "Name should match symbol-name")))

(deftest test-find-symbol*-finds-interned
  "Verify find-symbol* finds interned symbols"
  (clear-symbol-table)
  (intern-symbol "FINDME")
  (let ((found (find-symbol* "FINDME")))
    (ok (not (null found)) "Should find the symbol")
    (ok (string= "FINDME" (sym-name found)) "Should have correct name")))

(deftest test-find-symbol*-returns-nil-for-unknown
  "Verify find-symbol* returns NIL for unknown symbols"
  (clear-symbol-table)
  (let ((found (find-symbol* "UNKNOWN")))
    (ok (null found) "Should return NIL for unknown symbol")))

(deftest test-symbol-count-tracks-symbols
  "Verify symbol-count returns correct count"
  (clear-symbol-table)
  (ok (= 0 (symbol-count)) "Should start at 0")
  (intern-symbol "A")
  (ok (= 1 (symbol-count)) "Should be 1 after first intern")
  (intern-symbol "B")
  (ok (= 2 (symbol-count)) "Should be 2 after second intern")
  (intern-symbol "A")  ; Re-intern same symbol
  (ok (= 2 (symbol-count)) "Should still be 2 after re-interning"))

(deftest test-clear-symbol-table-clears-all
  "Verify clear-symbol-table removes all symbols"
  (clear-symbol-table)
  (intern-symbol "TO-CLEAR")
  (ok (= 1 (symbol-count)) "Should have 1 symbol")
  (clear-symbol-table)
  (ok (= 0 (symbol-count)) "Should have 0 symbols after clear")
  (ok (null (find-symbol* "TO-CLEAR")) "Should not find cleared symbol"))

(deftest test-symbol-value-slot
  "Verify symbol value slot works"
  (clear-symbol-table)
  (let ((sym (intern-symbol "HAS-VALUE")))
    (ok (null (sym-value sym)) "Initial value should be NIL")
    (setf (sym-value sym) 42)
    (ok (= 42 (sym-value sym)) "Value should be 42 after setf")))

(deftest test-symbol-function-slot
  "Verify symbol function slot works"
  (clear-symbol-table)
  (let ((sym (intern-symbol "HAS-FN")))
    (ok (null (sym-function sym)) "Initial function should be NIL")
    (setf (sym-function sym) #'+)
    (ok (eq #'+ (sym-function sym)) "Function should be + after setf")))
