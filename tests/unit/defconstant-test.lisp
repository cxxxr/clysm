;;; tests/unit/defconstant-test.lisp - Unit tests for defconstant/defparameter
;;; Feature 038: Stage 0 Capability Extension
;;; TDD: Tests written FIRST, must FAIL before implementation

(in-package #:clysm/tests/unit/defconstant)

;;;; Test Suite Definition
(deftest defconstant-tests
  (testing "User Story 1: Compile Constant Definitions"
    ;; T013: ast-defconstant struct creation
    ;; T014: parse-defconstant-form
    ;; T015: simple defconstant with literal value
    ;; T016: defconstant with arithmetic expression
    ;; T017: defconstant referencing another constant
    ;; T018: defparameter mutable global
    ))

;;;; T013: ast-defconstant struct creation
(deftest ast-defconstant-struct-test
  (testing "ast-defconstant struct should be defined"
    ;; Will fail until ast-defconstant is defined in ast.lisp
    (ok (fboundp 'clysm::make-ast-defconstant)
        "make-ast-defconstant should be defined")
    (ok (fboundp 'clysm::ast-defconstant-name)
        "ast-defconstant-name accessor should be defined")
    (ok (fboundp 'clysm::ast-defconstant-value-form)
        "ast-defconstant-value-form accessor should be defined")
    (ok (fboundp 'clysm::ast-defconstant-docstring)
        "ast-defconstant-docstring accessor should be defined")))

;;;; T014: parse-defconstant-form
(deftest parse-defconstant-form-test
  (testing "parse-defconstant-form should parse defconstant forms"
    ;; Will fail until parse-defconstant-form is implemented
    (ok (fboundp 'clysm::parse-defconstant-form)
        "parse-defconstant-form should be defined")

    ;; Test basic parsing
    (let ((ast (clysm::parse-defconstant-form '(+max-stack+ 1000))))
      (ok (typep ast 'clysm::ast-defconstant)
          "Should return ast-defconstant struct")
      (ok (eq (clysm::ast-defconstant-name ast) '+max-stack+)
          "Name should be +max-stack+")
      (ok (clysm::ast-defconstant-value-form ast)
          "Value form should be present"))))

;;;; T015: simple defconstant with literal value
(deftest defconstant-literal-value-test
  (testing "defconstant with literal value compiles to immutable global"
    ;; Scenario 1 from spec.md:
    ;; Given: (defconstant +max-stack+ 1000)
    ;; Then: output Wasm contains global with value 1000
    (let* ((form '(defconstant +max-stack+ 1000))
           (ast (clysm::parse-form form))
           (wasm (clysm::compile-form ast)))
      (ok wasm "Should compile without error")
      ;; Check that global is in output
      (ok (member :global.set (alexandria:flatten wasm))
          "Output should contain global definition"))))

;;;; T016: defconstant with arithmetic expression
(deftest defconstant-arithmetic-expression-test
  (testing "defconstant with computed value uses constant folding"
    ;; Scenario 3 from spec.md:
    ;; Given: (defconstant +bytes+ (* 8 1024))
    ;; Then: value is computed at compile-time as 8192
    (let* ((form '(defconstant +bytes+ (* 8 1024)))
           ;; Fold the expression
           (folded (clysm::fold-constant-expression '(* 8 1024) nil)))
      (ok (= folded 8192)
          "Constant folding should compute (* 8 1024) = 8192"))))

;;;; T017: defconstant referencing another constant
(deftest defconstant-reference-test
  (testing "defconstant can reference previously defined constants"
    ;; Scenario 4 from spec.md:
    ;; Given: +max-stack+ = 1000
    ;; Given: (defconstant +doubled+ (* 2 +max-stack+))
    ;; Then: value resolves to 2000
    (let ((registry (make-hash-table :test 'eq)))
      ;; Register first constant
      (setf (gethash '+max-stack+ registry) 1000)
      ;; Fold expression with registry
      (multiple-value-bind (value foldable-p)
          (clysm::fold-constant-expression '(* 2 +max-stack+) registry)
        (ok foldable-p "Expression should be foldable")
        (ok (= value 2000) "Should resolve to 2000")))))

;;;; T018: defparameter mutable global
(deftest defparameter-mutable-global-test
  (testing "defparameter compiles to mutable global"
    ;; Scenario 2 from spec.md:
    ;; Given: (defparameter *debug-level* 0)
    ;; Then: output Wasm contains mutable global initialized to 0
    (let* ((form '(defparameter *debug-level* 0))
           (ast (clysm::parse-form form))
           (wasm (clysm::compile-form ast)))
      (ok wasm "Should compile without error")
      ;; defparameter should produce mutable global
      (ok (clysm::ast-defparameter-p ast)
          "AST should be defparameter type"))))
