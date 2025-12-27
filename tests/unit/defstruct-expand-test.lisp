;;; tests/unit/defstruct-expand-test.lisp - Unit tests for defstruct expansion
;;; Feature 038: Stage 0 Capability Extension
;;; TDD: Tests written FIRST, must FAIL before implementation

(in-package #:clysm/tests/unit/defstruct-expand)

;;;; Test Suite Definition
(deftest defstruct-expand-tests
  (testing "User Story 5: Handle defstruct via Expansion"
    ;; T064: expand-defstruct basic case
    ;; T065: defstruct with slot defaults
    ;; T066: defstruct with :constructor option
    ;; T067: defstruct accessor generation
    ;; T068: defstruct predicate generation
    ))

;;;; T064: expand-defstruct basic case
(deftest expand-defstruct-basic-test
  (testing "expand-defstruct should generate constructor and accessors"
    ;; Scenario 1 from spec.md:
    ;; Given: (defstruct point x y)
    ;; Then: generates make-point, point-x, point-y functions
    (ok (fboundp 'clysm::expand-defstruct)
        "expand-defstruct should be defined")

    (let ((expansion (clysm::expand-defstruct '(defstruct point x y))))
      (ok (listp expansion) "Expansion should be a list of forms")

      ;; Check for constructor
      (ok (some (lambda (form)
                  (and (eq (car form) 'defun)
                       (eq (cadr form) 'make-point)))
                expansion)
          "Should generate make-point constructor")

      ;; Check for accessors
      (ok (some (lambda (form)
                  (and (eq (car form) 'defun)
                       (eq (cadr form) 'point-x)))
                expansion)
          "Should generate point-x accessor")

      (ok (some (lambda (form)
                  (and (eq (car form) 'defun)
                       (eq (cadr form) 'point-y)))
                expansion)
          "Should generate point-y accessor"))))

;;;; T065: defstruct with slot defaults
(deftest defstruct-slot-defaults-test
  (testing "defstruct with slot defaults uses defaults in constructor"
    ;; Scenario 3 from spec.md:
    ;; Given: (defstruct point (x 0) (y 0))
    ;; Then: constructor uses specified defaults
    (let ((expansion (clysm::expand-defstruct '(defstruct point (x 0) (y 0)))))
      ;; Find the constructor
      (let ((constructor (find-if (lambda (form)
                                    (and (eq (car form) 'defun)
                                         (eq (cadr form) 'make-point)))
                                  expansion)))
        (ok constructor "Constructor should exist")
        ;; Lambda-list should have &key with defaults
        (let ((lambda-list (caddr constructor)))
          (ok (member '&key lambda-list)
              "Constructor should have &key parameters")
          ;; Default values should be present
          (ok (or (member '(x 0) lambda-list :test #'equal)
                  (some (lambda (item)
                          (and (consp item)
                               (eq (car item) 'x)
                               (= (cadr item) 0)))
                        lambda-list))
              "x should default to 0"))))))

;;;; T066: defstruct with :constructor option
(deftest defstruct-constructor-option-test
  (testing "defstruct with :constructor option uses custom name"
    ;; Scenario 2 from spec.md:
    ;; Given: (defstruct (point (:constructor create-point)) x y)
    ;; Then: custom constructor name is used
    (let ((expansion (clysm::expand-defstruct
                      '(defstruct (point (:constructor create-point)) x y))))
      ;; Should have create-point, not make-point
      (ok (some (lambda (form)
                  (and (eq (car form) 'defun)
                       (eq (cadr form) 'create-point)))
                expansion)
          "Should generate create-point constructor")

      (ok (not (some (lambda (form)
                       (and (eq (car form) 'defun)
                            (eq (cadr form) 'make-point)))
                     expansion))
          "Should NOT generate make-point"))))

;;;; T067: defstruct accessor generation
(deftest defstruct-accessor-generation-test
  (testing "defstruct generates correct accessor implementations"
    (let ((expansion (clysm::expand-defstruct '(defstruct point x y))))
      ;; Find point-x accessor
      (let ((accessor-x (find-if (lambda (form)
                                   (and (eq (car form) 'defun)
                                        (eq (cadr form) 'point-x)))
                                 expansion)))
        (ok accessor-x "point-x accessor should exist")
        ;; Should take one argument (the struct instance)
        (let ((lambda-list (caddr accessor-x)))
          (ok (= (length lambda-list) 1)
              "Accessor should take exactly one argument"))))))

;;;; T068: defstruct predicate generation
(deftest defstruct-predicate-generation-test
  (testing "defstruct generates predicate function"
    (let ((expansion (clysm::expand-defstruct '(defstruct point x y))))
      ;; Should generate point-p predicate
      (ok (some (lambda (form)
                  (and (eq (car form) 'defun)
                       (eq (cadr form) 'point-p)))
                expansion)
          "Should generate point-p predicate"))))
