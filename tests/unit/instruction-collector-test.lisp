;;;; Instruction Collector Unit Tests
;;;; Tests for with-instruction-collector macro and emit/emit* local macros
;;;; Following TDD: These tests are written FIRST, before implementation

(defpackage #:clysm/tests/unit/instruction-collector-test
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:with-instruction-collector))

(in-package #:clysm/tests/unit/instruction-collector-test)

;;; T007: Empty body returns nil
(deftest empty-body-returns-nil
  (testing "with-instruction-collector with empty body returns nil"
    (ok (null (with-instruction-collector)))))

;;; T008: Single emit returns single instruction
(deftest single-emit-returns-single-instruction
  (testing "emit with single instruction returns a list with that instruction"
    (let ((result (with-instruction-collector
                    (emit :local.get 0))))
      (ok (equal result '((:local.get 0)))))))

;;; T009: Multiple emits preserve order
(deftest multiple-emits-preserve-order
  (testing "multiple emit calls preserve instruction order"
    (let ((result (with-instruction-collector
                    (emit :i32.const 42)
                    (emit :local.set 0)
                    (emit :local.get 0))))
      (ok (equal result '((:i32.const 42)
                          (:local.set 0)
                          (:local.get 0)))))))

;;; T010: emit* with list adds all instructions in order
(deftest emit*-adds-all-instructions-in-order
  (testing "emit* adds all instructions from a list in order"
    (let* ((instructions '((:local.get 0) (:local.get 1) :i32.add))
           (result (with-instruction-collector
                     (emit :block '$start)
                     (emit* instructions)
                     (emit :end))))
      (ok (equal result '((:block $start)
                          (:local.get 0)
                          (:local.get 1)
                          :i32.add
                          :end))))))

;;; T011: emit* with nil is no-op
(deftest emit*-with-nil-is-noop
  (testing "emit* with nil adds nothing"
    (let ((result (with-instruction-collector
                    (emit :i32.const 1)
                    (emit* nil)
                    (emit :i32.const 2))))
      (ok (equal result '((:i32.const 1)
                          (:i32.const 2)))))))

;;; T012: Nested collectors are independent
(deftest nested-collectors-are-independent
  (testing "nested with-instruction-collector forms have independent state"
    (let ((result (with-instruction-collector
                    (emit :block '$outer)
                    (let ((inner (with-instruction-collector
                                   (emit :i32.const 99))))
                      (emit* inner))
                    (emit :end))))
      (ok (equal result '((:block $outer)
                          (:i32.const 99)
                          :end))))))

;;; Additional edge case: bare keyword instructions (no operands)
;;; These should emit as bare keywords, not wrapped in lists
(deftest bare-keyword-instructions
  (testing "bare keyword instructions emit as bare keywords"
    (let ((result (with-instruction-collector
                    (emit :ref.is_null)
                    (emit :drop))))
      (ok (equal result '(:ref.is_null :drop))))))

;;; Additional edge case: mixed emit and emit* calls
;;; Tests both compound (list) and simple (bare keyword) instruction formats
(deftest mixed-emit-and-emit*
  (testing "mixed emit and emit* calls preserve order with correct formats"
    (let ((result (with-instruction-collector
                    (emit :block '$test)
                    (emit* '((:i32.const 1) (:i32.const 2)))
                    (emit :i32.add)
                    (emit* '((:local.set 0)))
                    (emit :end))))
      (ok (equal result '((:block $test)
                          (:i32.const 1)
                          (:i32.const 2)
                          :i32.add
                          (:local.set 0)
                          :end))))))
