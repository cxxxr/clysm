;;;; special-vars-codegen-test.lisp - Code generation contract tests (T012-T013)
(in-package #:clysm/tests/contract/special-vars-codegen)

;;; T012: Contract test for compile-defvar output
;;; Verifies that compile-defvar generates the correct Wasm instruction pattern

(deftest test-compile-defvar-contract
  (testing "defvar without init generates no-op"
    ;; (defvar *x*) should generate minimal code since symbol is created elsewhere
    ;; Expected: just nop or minimal registration code
    (let ((instrs (clysm/compiler/codegen/func-section:compile-expression
                   '(defvar *x*)
                   (clysm/compiler/codegen/func-section:make-env))))
      (ok instrs "Should generate instructions")))

  (testing "defvar with init generates conditional initialization"
    ;; (defvar *x* 10) should generate:
    ;; 1. Get symbol's current value
    ;; 2. Check if UNBOUND
    ;; 3. If unbound, set to init value
    (let ((instrs (clysm/compiler/codegen/func-section:compile-expression
                   '(defvar *x* 10)
                   (clysm/compiler/codegen/func-section:make-env))))
      (ok instrs "Should generate instructions")
      ;; Verify structure contains global.get and struct.get for UNBOUND check
      (ok (member :global.get (alexandria:flatten instrs))
          "Should access global symbol")
      (ok (member :struct.get (alexandria:flatten instrs))
          "Should get struct field (value slot)")
      ;; Should have conditional (if/br_if)
      (ok (or (member :if (alexandria:flatten instrs))
              (member :br_if (alexandria:flatten instrs)))
          "Should have conditional for UNBOUND check"))))

;;; T013: Contract test for compile-defparameter output
;;; Verifies that compile-defparameter always initializes

(deftest test-compile-defparameter-contract
  (testing "defparameter generates unconditional initialization"
    ;; (defparameter *x* 10) should always set the value, no UNBOUND check
    (let ((instrs (clysm/compiler/codegen/func-section:compile-expression
                   '(defparameter *x* 10)
                   (clysm/compiler/codegen/func-section:make-env))))
      (ok instrs "Should generate instructions")
      ;; Should access global symbol
      (ok (member :global.get (alexandria:flatten instrs))
          "Should access global symbol")
      ;; Should set struct field (value slot)
      (ok (member :struct.set (alexandria:flatten instrs))
          "Should set struct field (value slot)")
      ;; Should NOT have conditional UNBOUND check (unlike defvar)
      ;; The pattern is: global.get symbol, compile-value, struct.set
      (ok (not (member :ref.eq (alexandria:flatten instrs)))
          "Should NOT check for UNBOUND (always sets)"))))

;;; T029: Contract test for compile-let special binding codegen
;;; (Later phase, placeholder)

(deftest test-compile-let-special-binding-contract
  (testing "let with special binding generates save/restore pattern"
    ;; This test will be filled in Phase 4 (US2)
    (ok t "Placeholder for US2")))

;;; T037-T038: Contract tests for compile-var-ref and compile-setq special paths
;;; (Later phase, placeholders)

(deftest test-compile-special-var-ref-contract
  (testing "special variable reference generates symbol-value access"
    ;; This test will be filled in Phase 5 (US3)
    (ok t "Placeholder for US3")))

(deftest test-compile-special-setq-contract
  (testing "setq of special variable generates symbol-value write"
    ;; This test will be filled in Phase 5 (US3)
    (ok t "Placeholder for US3")))
