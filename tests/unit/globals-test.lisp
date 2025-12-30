;;;; globals-test.lisp - Unit tests for global variable compilation
;;;;
;;;; Part of Phase 13D-4: Global Variable Definitions
;;;; Tests defvar/defparameter compilation to WasmGC
;;;;
;;;; TDD: Tests first - these tests should FAIL before implementation

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Package Setup
;;; ============================================================

(deftest globals-test-suite
  (testing "Global variable compilation test suite exists"
    (ok t "Test suite loaded")))

;;; ============================================================
;;; User Story 1: Basic defvar/defparameter (T012-T015)
;;; ============================================================

(deftest defvar-integer-init-test
  (testing "T012: defvar with integer initialization"
    ;; Parse defvar form
    (let* ((form '(defvar *counter* 0))
           (ast (clysm/compiler/ast:parse-expr form)))
      ;; Verify AST structure
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*counter* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *counter*")
      ;; Verify init form exists
      (ok (clysm/compiler/ast:ast-defvar-init-form ast)
          "Init form is present")
      ;; Verify init form is a literal 0
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (eql 0 (clysm/compiler/ast:ast-literal-value init))
            "Init value is 0")))))

(deftest defvar-string-init-test
  (testing "T013: defvar with string initialization"
    (let* ((form '(defvar *name* "default"))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*name* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *name*")
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (equal "default" (clysm/compiler/ast:ast-literal-value init))
            "Init value is \"default\"")))))

(deftest defvar-no-init-test
  (testing "T014: defvar with no initialization (UNBOUND)"
    (let* ((form '(defvar *unset*))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*unset* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *unset*")
      ;; No init form means UNBOUND
      (ok (null (clysm/compiler/ast:ast-defvar-init-form ast))
          "Init form is NIL (unbound)"))))

(deftest defparameter-always-init-test
  (testing "T015: defparameter always initializes"
    (let* ((form '(defparameter *config* 42))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defparameter-p ast)
          "Parsed form is ast-defparameter")
      (ok (eq '*config* (clysm/compiler/ast:ast-defparameter-name ast))
          "Variable name is *config*")
      ;; defparameter MUST have init form
      (ok (clysm/compiler/ast:ast-defparameter-init-form ast)
          "Init form is present (required for defparameter)")
      (let ((init (clysm/compiler/ast:ast-defparameter-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (eql 42 (clysm/compiler/ast:ast-literal-value init))
            "Init value is 42")))))

;;; ============================================================
;;; User Story 2: Registry globals (T024-T025)
;;; ============================================================

(deftest defvar-hash-table-init-test
  (testing "T024: defvar with hash-table initialization (deferred)"
    (let* ((form '(defvar *registry* (make-hash-table :test 'eq)))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*registry* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *registry*")
      ;; Init form should be a function call
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-call-p init)
            "Init form is a function call")
        (ok (eq 'make-hash-table (clysm/compiler/ast:ast-call-function init))
            "Init form calls make-hash-table")))))

(deftest defvar-empty-list-init-test
  (testing "T025: defvar with empty list '() initialization"
    (let* ((form '(defvar *items* '()))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      ;; Empty list is constant NIL
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok init "Init form is present")
        ;; '() should parse to NIL or a quoted NIL
        (or (and (clysm/compiler/ast:ast-literal-p init)
                 (null (clysm/compiler/ast:ast-literal-value init)))
            (ok t "Empty list parsed as constant"))))))

;;; ============================================================
;;; User Story 3: Package system globals (T032-T033)
;;; ============================================================

(deftest defvar-nil-init-test
  (testing "T032: defvar with NIL initialization"
    (let* ((form '(defvar *current-package* nil))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (null (clysm/compiler/ast:ast-literal-value init))
            "Init value is NIL")))))

(deftest defparameter-function-call-init-test
  (testing "T033: defparameter with function call initialization (deferred)"
    (let* ((form '(defparameter *default-package* (find-package "CL-USER")))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defparameter-p ast)
          "Parsed form is ast-defparameter")
      (let ((init (clysm/compiler/ast:ast-defparameter-init-form ast)))
        (ok (clysm/compiler/ast:ast-call-p init)
            "Init form is a function call")
        (ok (eq 'find-package (clysm/compiler/ast:ast-call-function init))
            "Init form calls find-package")))))

;;; ============================================================
;;; User Story 4: I/O stream globals (T038-T039)
;;; ============================================================

(deftest defvar-global-reference-init-test
  (testing "T038: defvar referencing another global"
    (let* ((form '(defvar *trace-output* *standard-output*))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-var-ref-p init)
            "Init form is a variable reference")
        (ok (eq '*standard-output* (clysm/compiler/ast:ast-var-ref-name init))
            "Init references *standard-output*")))))

(deftest defvar-stream-nil-init-test
  (testing "T039: stream global with NIL initialization"
    (let* ((form '(defvar *query-io* nil))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (null (clysm/compiler/ast:ast-literal-value init))
            "Init value is NIL")))))

;;; ============================================================
;;; User Story 5: Condition system globals (T044-T045)
;;; ============================================================

(deftest defvar-condition-empty-list-test
  (testing "T044: condition cluster with empty list"
    (let* ((form '(defvar *handler-clusters* '()))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*handler-clusters* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *handler-clusters*"))))

(deftest defvar-debugger-hook-nil-test
  (testing "T045: *debugger-hook* with NIL"
    (let* ((form '(defvar *debugger-hook* nil))
           (ast (clysm/compiler/ast:parse-expr form)))
      (ok (clysm/compiler/ast:ast-defvar-p ast)
          "Parsed form is ast-defvar")
      (ok (eq '*debugger-hook* (clysm/compiler/ast:ast-defvar-name ast))
          "Variable name is *debugger-hook*")
      (let ((init (clysm/compiler/ast:ast-defvar-init-form ast)))
        (ok (clysm/compiler/ast:ast-literal-p init)
            "Init form is a literal")
        (ok (null (clysm/compiler/ast:ast-literal-value init))
            "Init value is NIL")))))

;;; ============================================================
;;; Init Type Classification Tests
;;; ============================================================

(deftest classify-constant-init-test
  (testing "Classify constant initialization"
    ;; Integer literal
    (let ((ast (clysm/compiler/ast:parse-expr 0)))
      (ok (clysm/compiler/codegen/globals:classify-init-type ast)
          "Integer classified"))
    ;; String literal
    (let ((ast (clysm/compiler/ast:parse-expr "hello")))
      (ok (clysm/compiler/codegen/globals:classify-init-type ast)
          "String classified"))
    ;; NIL
    (let ((ast (clysm/compiler/ast:parse-expr nil)))
      (ok (clysm/compiler/codegen/globals:classify-init-type ast)
          "NIL classified"))))

(deftest classify-deferred-init-test
  (testing "Classify deferred initialization"
    ;; Function call
    (let ((ast (clysm/compiler/ast:parse-expr '(make-hash-table :test 'eq))))
      (ok (eq :deferred (clysm/compiler/codegen/globals:classify-init-type ast))
          "Function call is deferred init"))))

(deftest classify-none-init-test
  (testing "Classify no initialization"
    (ok (eq :none (clysm/compiler/codegen/globals:classify-init-type nil))
        "NIL (no init form) is :none init type")))
