;;;; special-vars-ast-test.lisp - AST parsing tests for special variables (T010-T011)
(in-package #:clysm/tests/unit/special-vars-ast)

;;; T010: Unit test for ast-defvar parsing

(deftest test-defvar-parsing
  (testing "defvar with init-form parses correctly"
    (let ((ast (parse-expr '(defvar *x* 10))))
      (ok (typep ast 'ast-defvar) "Should create ast-defvar node")
      (ok (eq (ast-defvar-name ast) '*x*) "Name should be *x*")
      (ok (ast-defvar-init-form ast) "Init-form should be present")
      (ok (typep (ast-defvar-init-form ast) 'ast-literal)
          "Init-form should be a literal")))

  (testing "defvar without init-form parses correctly"
    (let ((ast (parse-expr '(defvar *unbound*))))
      (ok (typep ast 'ast-defvar) "Should create ast-defvar node")
      (ok (eq (ast-defvar-name ast) '*unbound*) "Name should be *unbound*")
      (ok (null (ast-defvar-init-form ast)) "Init-form should be nil")))

  (testing "defvar with docstring parses correctly"
    (let ((ast (parse-expr '(defvar *documented* 42 "A documented variable"))))
      (ok (typep ast 'ast-defvar) "Should create ast-defvar node")
      (ok (eq (ast-defvar-name ast) '*documented*) "Name should be *documented*")
      (ok (ast-defvar-init-form ast) "Init-form should be present")
      (ok (equal (ast-defvar-docstring ast) "A documented variable")
          "Docstring should be preserved")))

  (testing "defvar registers symbol as special"
    (clear-special-variables)
    (parse-expr '(defvar *registered* 1))
    (ok (special-variable-p '*registered*)
        "Symbol should be registered as special after parsing")))

;;; T011: Unit test for ast-defparameter parsing

(deftest test-defparameter-parsing
  (testing "defparameter with init-form parses correctly"
    (let ((ast (parse-expr '(defparameter *param* 20))))
      (ok (typep ast 'ast-defparameter) "Should create ast-defparameter node")
      (ok (eq (ast-defparameter-name ast) '*param*) "Name should be *param*")
      (ok (ast-defparameter-init-form ast) "Init-form should be present")))

  (testing "defparameter with docstring parses correctly"
    (let ((ast (parse-expr '(defparameter *param-doc* 30 "Param docs"))))
      (ok (typep ast 'ast-defparameter) "Should create ast-defparameter node")
      (ok (equal (ast-defparameter-docstring ast) "Param docs")
          "Docstring should be preserved")))

  (testing "defparameter registers symbol as special"
    (clear-special-variables)
    (parse-expr '(defparameter *reg-param* 1))
    (ok (special-variable-p '*reg-param*)
        "Symbol should be registered as special after parsing"))

  (testing "defparameter without init-form should error"
    ;; defparameter requires an initial value unlike defvar
    (ok (signals (parse-expr '(defparameter *no-init*)))
        "defparameter without init-form should signal an error")))
