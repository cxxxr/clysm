;;;; env-test.lisp - Unit tests for Stage 0 environment management
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Tests T005: Environment extend-env and lookup functions
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(defpackage #:clysm/tests/unit/stage0/env-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:make-env
                #:extend-env
                #:lookup
                #:empty-env-p))

(in-package #:clysm/tests/unit/stage0/env-test)

;;; ============================================================
;;; T005: Environment Infrastructure Tests
;;; ============================================================

(deftest test-make-env-creates-empty-environment
  "Verify make-env creates an empty environment"
  (let ((env (make-env)))
    (ok (empty-env-p env) "Fresh environment should be empty")))

(deftest test-extend-env-adds-binding
  "Verify extend-env adds a variable binding to environment"
  (let* ((env (make-env))
         (env2 (extend-env 'x 42 env)))
    (ok (not (eq env env2)) "extend-env should return new environment")
    (ok (not (empty-env-p env2)) "Extended environment should not be empty")))

(deftest test-lookup-finds-binding
  "Verify lookup retrieves bound value"
  (let* ((env (make-env))
         (env2 (extend-env 'x 42 env)))
    (multiple-value-bind (value found-p) (lookup 'x env2)
      (ok found-p "Should find binding for x")
      (ok (eql value 42) "Should return correct value 42"))))

(deftest test-lookup-returns-nil-for-unbound
  "Verify lookup returns nil/false for unbound variable"
  (let ((env (make-env)))
    (multiple-value-bind (value found-p) (lookup 'x env)
      (ok (not found-p) "Should not find binding for x")
      (ok (null value) "Value should be nil for unbound"))))

(deftest test-extend-env-multiple-bindings
  "Verify multiple extend-env calls create proper shadowing"
  (let* ((env (make-env))
         (env2 (extend-env 'x 1 env))
         (env3 (extend-env 'y 2 env2))
         (env4 (extend-env 'x 100 env3)))  ; shadow x
    (multiple-value-bind (val-x found-x) (lookup 'x env4)
      (ok found-x "Should find x")
      (ok (eql val-x 100) "x should be shadowed to 100"))
    (multiple-value-bind (val-y found-y) (lookup 'y env4)
      (ok found-y "Should find y")
      (ok (eql val-y 2) "y should be 2"))))

(deftest test-extend-env-batch
  "Verify extend-env can handle batch binding (for let)"
  (let* ((env (make-env))
         (env2 (extend-env 'a 10 env))
         (env3 (extend-env 'b 20 env2))
         (env4 (extend-env 'c 30 env3)))
    (ok (eql (values (lookup 'a env4)) 10) "a should be 10")
    (ok (eql (values (lookup 'b env4)) 20) "b should be 20")
    (ok (eql (values (lookup 'c env4)) 30) "c should be 30")))

(deftest test-original-env-unchanged
  "Verify extend-env doesn't mutate original environment"
  (let* ((env (make-env))
         (env2 (extend-env 'x 42 env)))
    (declare (ignore env2))
    (multiple-value-bind (value found-p) (lookup 'x env)
      (ok (not found-p) "Original env should not have x binding"))))

;;; ============================================================
;;; Environment representation tests (association list based)
;;; ============================================================

(deftest test-env-is-list-based
  "Verify environment uses list representation per research.md"
  (let* ((env (make-env))
         (env2 (extend-env 'x 42 env)))
    (ok (listp env) "Empty env should be a list")
    (ok (listp env2) "Extended env should be a list")))
