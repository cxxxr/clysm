;;;; analyzer-test.lisp - Analyzer tests (T070)
(in-package #:clysm/tests/unit/analyzer)

;;; T070: Free variable analysis tests
;;; Tests for collecting free variables from lambda expressions

(deftest test-free-vars-collector-exists
  "Free variable collector function should exist"
  (ok (fboundp 'clysm/compiler/analyzer/free-vars:collect-free-variables)
      "collect-free-variables should be defined"))

(deftest test-no-free-vars-simple-lambda
  "Lambda with no external references has no free variables"
  ;; (lambda (x) x) - x is bound, not free
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) x)))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (null free-vars) "Simple identity lambda should have no free variables")))

(deftest test-single-free-var
  "Lambda referencing external variable has one free variable"
  ;; In context where y is bound: (lambda (x) (+ x y))
  ;; y is free
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (+ x y))))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (member 'y free-vars) "y should be in free variables")))

(deftest test-multiple-free-vars
  "Lambda with multiple external references"
  ;; (lambda (x) (+ x y z)) - y and z are free
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (+ x y z))))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (member 'y free-vars) "y should be in free variables")
    (ok (member 'z free-vars) "z should be in free variables")
    (ok (not (member 'x free-vars)) "x is bound, not free")))

(deftest test-nested-lambda-free-vars
  "Nested lambda captures variables from outer scope"
  ;; (lambda (x) (lambda (y) (+ x y)))
  ;; Inner lambda has x as free variable
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (lambda (y) (+ x y)))))
         ;; Get the inner lambda
         (inner-lambda (car (clysm/compiler/ast:ast-lambda-body ast)))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables inner-lambda)))
    (ok (member 'x free-vars) "x should be free in inner lambda")))

(deftest test-let-bound-not-free
  "Variables bound by let inside lambda are not free"
  ;; (lambda (x) (let ((y 1)) (+ x y)))
  ;; y is bound by let, not free
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (let ((y 1)) (+ x y)))))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (not (member 'y free-vars)) "y is bound by let, not free")))

(deftest test-shadowed-variable
  "Shadowed variables handled correctly"
  ;; (lambda (x) (let ((x 1)) x))
  ;; Inner x shadows outer, no free variables
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (let ((x 1)) x))))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (not (member 'x free-vars)) "x is shadowed and bound, not free")))

(deftest test-free-vars-exclude-primitives
  "Primitive operations (+, -, etc.) are not free variables"
  ;; (lambda (x) (+ x 1))
  ;; + is a primitive, not a free variable
  (let* ((ast (clysm/compiler/ast:parse-expr '(lambda (x) (+ x 1))))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast)))
    (ok (not (member '+ free-vars)) "+ should not be in free variables")))
