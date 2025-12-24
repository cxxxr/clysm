;;;; condition-types-test.lisp - Tests for condition class hierarchy
;;;; TDD: These tests define expected behavior BEFORE implementation

(in-package #:clysm/tests)

(deftest condition-base-class-test
  "Test that condition is the base class for all conditions (FR-001)"
  ;; condition should be a class
  (ok (find-class 'clysm/conditions:condition nil)
      "condition class should exist")
  ;; All conditions should be subtypes of condition
  (ok (subtypep 'clysm/conditions:serious-condition 'clysm/conditions:condition)
      "serious-condition should be a subtype of condition")
  (ok (subtypep 'clysm/conditions:warning 'clysm/conditions:condition)
      "warning should be a subtype of condition"))

(deftest serious-condition-test
  "Test serious-condition class hierarchy (FR-002)"
  (ok (find-class 'clysm/conditions:serious-condition nil)
      "serious-condition class should exist")
  (ok (subtypep 'clysm/conditions:error 'clysm/conditions:serious-condition)
      "error should be a subtype of serious-condition"))

(deftest error-class-test
  "Test error class hierarchy (FR-003)"
  (ok (find-class 'clysm/conditions:error nil)
      "error class should exist")
  ;; error is a subclass of serious-condition
  (ok (subtypep 'clysm/conditions:error 'clysm/conditions:serious-condition)
      "error should be a subtype of serious-condition")
  ;; error is a subclass of condition
  (ok (subtypep 'clysm/conditions:error 'clysm/conditions:condition)
      "error should be a subtype of condition"))

(deftest warning-class-test
  "Test warning class hierarchy (FR-004)"
  (ok (find-class 'clysm/conditions:warning nil)
      "warning class should exist")
  ;; warning is a direct subclass of condition, not serious-condition
  (ok (subtypep 'clysm/conditions:warning 'clysm/conditions:condition)
      "warning should be a subtype of condition")
  (ok (not (subtypep 'clysm/conditions:warning 'clysm/conditions:serious-condition))
      "warning should NOT be a subtype of serious-condition"))

(deftest simple-condition-mixin-test
  "Test simple-condition mixin with format slots (FR-005)"
  (ok (find-class 'clysm/conditions:simple-condition nil)
      "simple-condition class should exist")
  ;; Create an instance and verify slot accessors
  (let ((c (make-instance 'clysm/conditions:simple-condition
                          :format-control "Test: ~A"
                          :format-arguments '(42))))
    (ok (string= "Test: ~A" (clysm/conditions:simple-condition-format-control c))
        "format-control accessor should work")
    (ok (equal '(42) (clysm/conditions:simple-condition-format-arguments c))
        "format-arguments accessor should work")))

(deftest simple-error-test
  "Test simple-error combines simple-condition and error (FR-006)"
  (ok (find-class 'clysm/conditions:simple-error nil)
      "simple-error class should exist")
  (ok (subtypep 'clysm/conditions:simple-error 'clysm/conditions:simple-condition)
      "simple-error should be a subtype of simple-condition")
  (ok (subtypep 'clysm/conditions:simple-error 'clysm/conditions:error)
      "simple-error should be a subtype of error"))

(deftest simple-warning-test
  "Test simple-warning combines simple-condition and warning (FR-007)"
  (ok (find-class 'clysm/conditions:simple-warning nil)
      "simple-warning class should exist")
  (ok (subtypep 'clysm/conditions:simple-warning 'clysm/conditions:simple-condition)
      "simple-warning should be a subtype of simple-condition")
  (ok (subtypep 'clysm/conditions:simple-warning 'clysm/conditions:warning)
      "simple-warning should be a subtype of warning"))

(deftest type-error-test
  "Test type-error with datum and expected-type slots (FR-008)"
  (ok (find-class 'clysm/conditions:type-error nil)
      "type-error class should exist")
  (ok (subtypep 'clysm/conditions:type-error 'clysm/conditions:error)
      "type-error should be a subtype of error")
  ;; Create and verify slot accessors
  (let ((c (make-instance 'clysm/conditions:type-error
                          :datum "not-a-number"
                          :expected-type 'integer)))
    (ok (string= "not-a-number" (clysm/conditions:type-error-datum c))
        "datum accessor should work")
    (ok (eq 'integer (clysm/conditions:type-error-expected-type c))
        "expected-type accessor should work")))

(deftest cell-error-test
  "Test cell-error with name slot (FR-008 implied)"
  (ok (find-class 'clysm/conditions:cell-error nil)
      "cell-error class should exist")
  (ok (subtypep 'clysm/conditions:cell-error 'clysm/conditions:error)
      "cell-error should be a subtype of error")
  ;; Create and verify slot accessor
  (let ((c (make-instance 'clysm/conditions:cell-error :name 'foo)))
    (ok (eq 'foo (clysm/conditions:cell-error-name c))
        "name accessor should work")))

(deftest undefined-function-test
  "Test undefined-function with name slot (FR-009)"
  (ok (find-class 'clysm/conditions:undefined-function nil)
      "undefined-function class should exist")
  (ok (subtypep 'clysm/conditions:undefined-function 'clysm/conditions:cell-error)
      "undefined-function should be a subtype of cell-error")
  ;; Inherits name slot from cell-error
  (let ((c (make-instance 'clysm/conditions:undefined-function :name 'bar)))
    (ok (eq 'bar (clysm/conditions:cell-error-name c))
        "name accessor should work for undefined-function")))

(deftest unbound-variable-test
  "Test unbound-variable with name slot"
  (ok (find-class 'clysm/conditions:unbound-variable nil)
      "unbound-variable class should exist")
  (ok (subtypep 'clysm/conditions:unbound-variable 'clysm/conditions:cell-error)
      "unbound-variable should be a subtype of cell-error"))

(deftest control-error-test
  "Test control-error class"
  (ok (find-class 'clysm/conditions:control-error nil)
      "control-error class should exist")
  (ok (subtypep 'clysm/conditions:control-error 'clysm/conditions:error)
      "control-error should be a subtype of error"))

(deftest make-condition-test
  "Test make-condition function (T032)"
  ;; make-condition should create condition instances
  (let ((c (clysm/conditions:make-condition 'clysm/conditions:simple-error
                                            :format-control "Error: ~A"
                                            :format-arguments '(test))))
    (ok (typep c 'clysm/conditions:simple-error)
        "make-condition should create correct type")
    (ok (string= "Error: ~A" (clysm/conditions:simple-condition-format-control c))
        "make-condition should set slots correctly")))

(deftest simple-warning-format-arguments-test
  "Test simple-warning with format arguments (T050)"
  (testing "simple-warning stores format-control and format-arguments"
    (let ((w (clysm/conditions:make-condition 'clysm/conditions:simple-warning
                                              :format-control "Warning: ~A ~D"
                                              :format-arguments '("value" 42))))
      (ok (typep w 'clysm/conditions:simple-warning)
          "make-condition should create simple-warning")
      (ok (string= "Warning: ~A ~D"
                   (clysm/conditions:simple-condition-format-control w))
          "format-control should be stored")
      (ok (equal '("value" 42)
                 (clysm/conditions:simple-condition-format-arguments w))
          "format-arguments should be stored")))

  (testing "warn with string uses format-control"
    (let ((output (with-output-to-string (cl:*error-output*)
                    (clysm/conditions:warn "Test ~A ~D" "message" 123))))
      (ok (search "Test message 123" output)
          "warn should format message using arguments"))))
