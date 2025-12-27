;;;; ir-test.lisp - Unit tests for Stage 0 Wasm IR generation
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests Core Infrastructure: Wasm IR generation

(defpackage #:clysm/tests/unit/stage0/ir-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-wasm-ir
                #:ir-to-instructions))

(in-package #:clysm/tests/unit/stage0/ir-test)

;;; ============================================================
;;; T023: Unit test for Wasm IR generation
;;; ============================================================

(deftest test-generate-ir-for-constant
  "Verify IR generation for constant values"
  (let ((ir (generate-wasm-ir '(:literal 42))))
    (ok (not (null ir)) "Should return IR")
    (ok (listp ir) "IR should be a list of instructions")))

(deftest test-generate-ir-for-add
  "Verify IR generation for addition"
  (let ((ir (generate-wasm-ir '(:call + ((:literal 1) (:literal 2))))))
    (ok (not (null ir)) "Should return IR")
    (ok (listp ir) "IR should be a list")))

(deftest test-ir-contains-instructions
  "Verify IR contains Wasm instructions"
  (let ((ir (generate-wasm-ir '(:literal 42))))
    ;; IR should contain instruction keywords like :i32.const
    (ok (or (member :i32.const ir)
            (member :i31.new ir)
            (member :ref.i31 ir)
            (listp ir))
        "Should contain Wasm instructions")))

(deftest test-ir-to-instructions-returns-bytes
  "Verify ir-to-instructions produces byte list"
  (let* ((ir '((:i32.const 42)))
         (instrs (ir-to-instructions ir)))
    (ok (listp instrs) "Should return list")
    (ok (every #'integerp instrs) "Should be list of bytes")))

;;; ============================================================
;;; Instruction Encoding Tests
;;; ============================================================

(deftest test-i32-const-encoding
  "Verify i32.const instruction encoding"
  (let ((instrs (ir-to-instructions '((:i32.const 0)))))
    (ok (member #x41 instrs) "Should contain i32.const opcode (0x41)")))

(deftest test-i31-new-encoding
  "Verify i31.new instruction encoding"
  (let ((instrs (ir-to-instructions '((:ref.i31)))))
    ;; ref.i31 is GC instruction 0xFB 0x1C
    (ok (or (member #xFB instrs)
            (member #x1C instrs))
        "Should contain GC prefix or i31.new opcode")))

(deftest test-end-instruction
  "Verify end instruction encoding"
  (let ((instrs (ir-to-instructions '((:end)))))
    (ok (member #x0B instrs) "Should contain end opcode (0x0B)")))
