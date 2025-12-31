;;;; clos-primitives-wasm-test.lisp - Contract tests for CLOS primitives Wasm codegen
;;;; Feature: 001-m3-clos-primitives
;;;; Phase 13D M3: CLOS Primitives for Wasm

(in-package #:clysm/tests/contract/clos-primitives-wasm)

;;; Contract tests verify that CLOS primitive functions compile to valid Wasm:
;;; - slot-value* -> struct.get + array.get sequence
;;; - (setf slot-value*) -> array.set + struct.set sequence
;;; - make-instance* -> struct.new sequence
;;; - standard-instance-p -> ref.test opcode
;;;
;;; These primitives are foundational for DEFSTRUCT and DEFINE-CONDITION compilation.
;;; HyperSpec references:
;;; - slot-value: resources/HyperSpec/Body/f_slt_va.htm
;;; - make-instance: resources/HyperSpec/Body/f_mk_ins.htm

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun compile-validates (expr)
  "Compile expression and validate the resulting Wasm.
   Returns T if compilation succeeds and Wasm validates, NIL otherwise."
  (handler-case
      (let ((bytes (clysm/compiler:compile-to-wasm expr)))
        (clysm/tests:validate-wasm-silent bytes))
    (error () nil)))

(defun compile-succeeds (expr)
  "Check if expression compiles without error.
   Returns T if compilation succeeds, NIL otherwise."
  (handler-case
      (progn
        (clysm/compiler:compile-to-wasm expr)
        t)
    (error () nil)))

;;; ============================================================
;;; Phase 2: T005 - slot-value* Read Codegen (FR-001)
;;; ============================================================

(deftest slot-value-read-codegen
  "T005 [US3]: slot-value* read compiles to valid Wasm with struct.get + array.get"
  (testing "slot-value* call compiles successfully"
    ;; slot-value* is the internal primitive that takes instance, slot-name, and index
    ;; It should compile to: struct.get (get $slots from $instance) + array.get (get slot)
    (ok (compile-validates
         '(defun test-slot-read (obj)
            (clysm/clos/slot-access:slot-value* obj 'x 0)))
        "slot-value* should compile to valid Wasm"))

  (testing "slot-value* with different slot indices"
    (ok (compile-validates
         '(defun test-multi-slot (obj)
            (list (clysm/clos/slot-access:slot-value* obj 'x 0)
                  (clysm/clos/slot-access:slot-value* obj 'y 1))))
        "Multiple slot-value* calls should compile")))

;;; ============================================================
;;; Phase 2: T006 - slot-value* Write Codegen (FR-002)
;;; ============================================================

(deftest slot-value-write-codegen
  "T006 [US3]: (setf slot-value*) compiles to valid Wasm with array.set"
  (testing "(setf slot-value*) call compiles successfully"
    ;; (setf slot-value*) should compile to: array.set sequence
    (ok (compile-validates
         '(defun test-slot-write (obj val)
            (clysm/clos/slot-access:set-slot-value* obj 'x 0 val)))
        "(setf slot-value*) should compile to valid Wasm"))

  (testing "combined slot read and write"
    (ok (compile-validates
         '(defun test-slot-modify (obj)
            (clysm/clos/slot-access:set-slot-value*
              obj 'x 0
              (+ 1 (clysm/clos/slot-access:slot-value* obj 'x 0)))))
        "Read-modify-write pattern should compile")))

;;; ============================================================
;;; Phase 2: T007 - make-instance* Codegen (FR-003)
;;; ============================================================

(deftest make-instance-codegen
  "T007 [US3]: make-instance* compiles to valid Wasm with struct.new"
  (testing "make-instance* call compiles successfully"
    ;; make-instance* is the internal primitive for instance creation
    ;; It should compile to struct.new for $instance + array.new_default for $slot-vector
    (ok (compile-validates
         '(defun test-make-instance (class-info)
            (clysm/clos/instance:make-instance* class-info)))
        "make-instance* should compile to valid Wasm"))

  (testing "make-instance* with slot count"
    (ok (compile-validates
         '(defun test-make-with-slots (class-info slot-count)
            (clysm/clos/instance:make-instance* class-info slot-count)))
        "make-instance* with slot count should compile")))

;;; ============================================================
;;; Phase 2: T008 - standard-instance-p Codegen (FR-004)
;;; ============================================================

(deftest standard-instance-p-codegen
  "T008 [US3]: standard-instance-p compiles to valid Wasm with ref.test"
  (testing "standard-instance-p call compiles successfully"
    ;; standard-instance-p should compile to ref.test $instance
    (ok (compile-validates
         '(defun test-instance-p (obj)
            (clysm/clos/mop:standard-instance-p obj)))
        "standard-instance-p should compile to valid Wasm"))

  (testing "standard-instance-p in conditional"
    (ok (compile-validates
         '(defun test-instance-check (obj)
            (if (clysm/clos/mop:standard-instance-p obj)
                (clysm/clos/slot-access:slot-value* obj 'x 0)
                nil)))
        "standard-instance-p in if should compile")))

;;; ============================================================
;;; Combined Tests
;;; ============================================================

(deftest clos-primitives-combined
  "Combined CLOS primitives compile together"
  (testing "Full slot access pattern"
    (ok (compile-validates
         '(defun test-full-access (class-info)
            (let ((instance (clysm/clos/instance:make-instance* class-info 2)))
              (clysm/clos/slot-access:set-slot-value* instance 'x 0 10)
              (clysm/clos/slot-access:set-slot-value* instance 'y 1 20)
              (+ (clysm/clos/slot-access:slot-value* instance 'x 0)
                 (clysm/clos/slot-access:slot-value* instance 'y 1)))))
        "Full instance creation and slot access should compile"))

  (testing "Type checking with slot access"
    (ok (compile-validates
         '(defun test-safe-access (obj)
            (when (clysm/clos/mop:standard-instance-p obj)
              (clysm/clos/slot-access:slot-value* obj 'value 0))))
        "Type-guarded slot access should compile")))

;;; ============================================================
;;; Edge Case Tests (to be expanded in Phase 5)
;;; ============================================================

(deftest clos-primitives-edge-cases
  "Edge cases for CLOS primitives"
  (skip "Edge case tests - to be implemented in Phase 5 (T047-T048)"))
