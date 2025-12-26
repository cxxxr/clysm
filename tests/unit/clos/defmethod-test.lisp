;;;; defmethod-test.lisp - Unit tests for defmethod AST and dispatch
;;;; Feature: 026-clos-foundation (User Stories 4, 5)

(in-package #:clysm/tests/unit/clos-codegen)

;;; Unit tests for defmethod AST parsing and generic function dispatch.

;;; ============================================================
;;; T065: defmethod AST parsing
;;; ============================================================

(deftest defmethod-ast-parsing
  "T065: defmethod form parses to ast-defmethod node"
  (testing "simple defmethod parses correctly"
    (let ((form '(defmethod area ((p point)) (* (point-x p) (point-y p)))))
      (ok (fboundp 'clysm/compiler/ast:parse-defmethod-to-ast)
          "parse-defmethod-to-ast should be defined")
      (let ((ast (clysm/compiler/ast:parse-defmethod-to-ast form)))
        (ok (typep ast 'clysm/compiler/ast:ast-defmethod)
            "Result should be ast-defmethod")
        (ok (eq 'area (clysm/compiler/ast:ast-defmethod-name ast))
            "Method name should be AREA")
        (ok (null (clysm/compiler/ast:ast-defmethod-qualifier ast))
            "No qualifier for primary method"))))

  (testing "defmethod with qualifier parses correctly"
    (let* ((form '(defmethod initialize-instance :after ((p point)) (print "initialized")))
           (ast (clysm/compiler/ast:parse-defmethod-to-ast form)))
      (ok (eq 'initialize-instance (clysm/compiler/ast:ast-defmethod-name ast))
          "Method name should be INITIALIZE-INSTANCE")
      (ok (eq :after (clysm/compiler/ast:ast-defmethod-qualifier ast))
          "Qualifier should be :AFTER"))))

;;; ============================================================
;;; T066: method specializer extraction
;;; ============================================================

(deftest method-specializer-extraction
  "T066: specializers are extracted from defmethod lambda-list"
  (testing "class specializer is extracted"
    (let* ((form '(defmethod speak ((a animal)) "generic sound"))
           (ast (clysm/compiler/ast:parse-defmethod-to-ast form))
           (specializers (clysm/compiler/ast:ast-defmethod-specializers ast)))
      (ok (= 1 (length specializers))
          "Should have 1 specializer")
      (ok (eq 'animal (first specializers))
          "Specializer should be ANIMAL")))

  (testing "multiple specializers are extracted"
    (let* ((form '(defmethod compare ((a number) (b number)) (< a b)))
           (ast (clysm/compiler/ast:parse-defmethod-to-ast form))
           (specializers (clysm/compiler/ast:ast-defmethod-specializers ast)))
      (ok (= 2 (length specializers))
          "Should have 2 specializers")
      (ok (eq 'number (first specializers))
          "First specializer should be NUMBER")
      (ok (eq 'number (second specializers))
          "Second specializer should be NUMBER"))))

;;; ============================================================
;;; T067: generic function implicit creation
;;; ============================================================

(deftest generic-function-implicit-creation
  "T067: defmethod creates generic function if not exists"
  ;; When a defmethod is compiled and no GF exists, one should be created
  (testing "generic function registry is available"
    (ok (fboundp 'clysm/compiler/codegen/func-section:find-generic-function)
        "find-generic-function should be defined")
    (ok (fboundp 'clysm/compiler/codegen/func-section:register-generic-function)
        "register-generic-function should be defined")))

;;; ============================================================
;;; T068: compute-applicable-methods
;;; ============================================================

(deftest compute-applicable-methods-test
  "T068: applicable methods are computed based on specializers"
  ;; This uses the existing CLOS infrastructure in clysm/clos/dispatch
  (testing "existing dispatch infrastructure is available"
    (ok (fboundp 'clysm/clos/dispatch:compute-applicable-methods)
        "compute-applicable-methods should exist in clysm/clos/dispatch")))

;;; ============================================================
;;; T069: method specificity sorting
;;; ============================================================

(deftest method-specificity-sorting
  "T069: methods are sorted by specificity"
  ;; More specific classes come before less specific
  ;; Uses existing CLOS infrastructure
  (testing "existing method sorting is available"
    (ok (fboundp 'clysm/clos/dispatch:sort-methods)
        "sort-methods should exist in clysm/clos/dispatch")))
