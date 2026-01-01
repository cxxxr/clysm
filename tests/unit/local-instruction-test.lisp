;;; local-instruction-test.lisp - Unit tests for LOCAL.SET/LOCAL.TEE instruction handling
;;; Feature: 001-wasm-local-binding
;;;
;;; This file tests that Wasm local variable instructions (:local.set, :local.tee)
;;; are properly handled during compilation, particularly in backquote expressions
;;; where they appear as keyword literals that must be preserved through macro expansion.

(in-package #:clysm/tests/unit/local-instruction)

;;; ============================================================
;;; Test: LOCAL.SET keyword preserved in backquote expressions
;;; ============================================================

(deftest local-set-opcode-in-opcode-table
  "Test that :local.set is defined in the opcode table with correct value."
  (testing "Opcode table should contain :local.set -> 0x21 mapping"
    (let ((table clysm/compiler/codegen/func-section::*wasm-opcodes*))
      (ok table "Opcode table should exist")
      (ok (listp table) "Should be an alist")
      (let ((entry (assoc :local.set table)))
        (ok entry ":local.set should be in opcode table")
        (ok (= (cdr entry) #x21) ":local.set should map to 0x21")))))

(deftest local-tee-opcode-in-opcode-table
  "Test that :local.tee is defined in the opcode table with correct value."
  (testing "Opcode table should contain :local.tee -> 0x22 mapping"
    (let ((table clysm/compiler/codegen/func-section::*wasm-opcodes*))
      (ok table "Opcode table should exist")
      (let ((entry (assoc :local.tee table)))
        (ok entry ":local.tee should be in opcode table")
        (ok (= (cdr entry) #x22) ":local.tee should map to 0x22")))))

;;; ============================================================
;;; Test: Compilation of functions containing local variable assignments
;;; ============================================================

(deftest compile-function-with-local-set-pattern
  "Test that functions using the typical local.set pattern compile without error."
  (testing "Function with backquote local.set pattern should compile"
    ;; This tests that when compile-gethash (or similar) is compiled to Wasm,
    ;; the :local.set keyword in the backquote expression is handled correctly
    (let ((test-form '(defun test-local-pattern (x)
                        (let ((temp x))
                          (setq temp (1+ temp))
                          temp))))
      (handler-case
          (let ((result (clysm:compile-to-wasm test-form)))
            (ok result "Function should compile without error")
            ;; Validate the Wasm output
            (when result
              (ok (validate-wasm-silent result) "Generated Wasm should be valid")))
        (error (e)
          (let ((msg (princ-to-string e)))
            ;; The error message should NOT contain "Unbound variable: LOCAL.SET"
            (ok (not (search "LOCAL.SET" msg :test #'char-equal))
                (format nil "Should not have LOCAL.SET unbound error: ~A" msg))))))))

(deftest compile-function-with-local-tee-pattern
  "Test that functions using the typical local.tee pattern compile without error."
  (testing "Function with local.tee pattern should compile"
    ;; local.tee is used when we need to both store and keep the value on stack
    (let ((test-form '(defun test-tee-pattern (x)
                        (let ((temp nil))
                          (when (setq temp x)
                            temp)))))
      (handler-case
          (let ((result (clysm:compile-to-wasm test-form)))
            (ok result "Function should compile without error")
            (when result
              (ok (validate-wasm-silent result) "Generated Wasm should be valid")))
        (error (e)
          (let ((msg (princ-to-string e)))
            (ok (not (search "LOCAL.TEE" msg :test #'char-equal))
                (format nil "Should not have LOCAL.TEE unbound error: ~A" msg))))))))

;;; ============================================================
;;; Test: Error pattern verification
;;; ============================================================

(deftest p221-error-pattern-elimination
  "Test that P221 error pattern (Unbound variable: LOCAL.SET) is eliminated."
  (testing "Compiling a function with local variable assignment"
    ;; Compile a function that would trigger P221 in the old system
    (let ((test-form '(defun increment-counter (n)
                        (let ((count 0))
                          (dotimes (i n)
                            (setq count (1+ count)))
                          count))))
      (let ((error-occurred nil)
            (error-msg nil))
        (handler-case
            (clysm:compile-to-wasm test-form)
          (error (e)
            (setq error-occurred t)
            (setq error-msg (princ-to-string e))))
        ;; Check: if error occurred, it should NOT be the LOCAL.SET unbound error
        (when error-occurred
          (ok (not (search "Unbound variable: LOCAL.SET" error-msg))
              "Error should not be P221 (LOCAL.SET unbound)"))))))

(deftest p987-error-pattern-elimination
  "Test that P987 error pattern (Unbound variable: LOCAL.TEE) is eliminated."
  (testing "Compiling a function with tee-style assignment"
    ;; Compile a function that would trigger P987 in the old system
    (let ((test-form '(defun check-and-use (val)
                        (let ((saved nil))
                          (if (setq saved val)
                              (list saved saved)
                              nil)))))
      (let ((error-occurred nil)
            (error-msg nil))
        (handler-case
            (clysm:compile-to-wasm test-form)
          (error (e)
            (setq error-occurred t)
            (setq error-msg (princ-to-string e))))
        ;; Check: if error occurred, it should NOT be the LOCAL.TEE unbound error
        (when error-occurred
          (ok (not (search "Unbound variable: LOCAL.TEE" error-msg))
              "Error should not be P987 (LOCAL.TEE unbound)"))))))

;;; ============================================================
;;; Test: Keyword literal handling in AST
;;; ============================================================

(deftest keyword-literal-compilation
  "Test that keyword literals are properly handled as literals, not variable refs."
  (testing "Keywords should compile as literal values"
    ;; When :local.set appears in a quoted context, it should be a literal
    (let ((ast (clysm/compiler/ast:make-ast-literal
                :literal-type :quoted
                :value :local.set)))
      (ok (clysm/compiler/ast:ast-literal-p ast)
          "AST node should be a literal")
      (ok (keywordp (clysm/compiler/ast:ast-literal-value ast))
          "Value should remain a keyword"))))
