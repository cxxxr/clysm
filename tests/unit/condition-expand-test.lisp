;;; tests/unit/condition-expand-test.lisp - Unit tests for define-condition expansion
;;; Feature 038: Stage 0 Capability Extension
;;; TDD: Tests written FIRST, must FAIL before implementation

(in-package #:clysm/tests/unit/condition-expand)

;;;; Test Suite Definition
(deftest condition-expand-tests
  (testing "User Story 2: Compile Condition Definitions"
    ;; T029: expand-define-condition basic case
    ;; T030: define-condition with slots
    ;; T031: define-condition with :report option
    ;; T032: nested condition inheritance
    ))

;;;; T029: expand-define-condition basic case
(deftest expand-define-condition-basic-test
  (testing "expand-define-condition converts to defclass"
    ;; Scenario 1 from spec.md:
    ;; Given: (define-condition my-error (error) ((msg :initarg :msg)))
    ;; Then: becomes (defclass my-error (error) ((msg :initarg :msg)))
    (ok (fboundp 'clysm::expand-define-condition)
        "expand-define-condition should be defined")

    (let ((expansion (clysm::expand-define-condition
                      '(define-condition my-error (error)
                        ((msg :initarg :msg))))))
      (ok (eq (car expansion) 'defclass)
          "Expansion should be defclass")
      (ok (eq (cadr expansion) 'my-error)
          "Name should be my-error")
      (ok (equal (caddr expansion) '(error))
          "Parents should be (error)"))))

;;;; T030: define-condition with slots
(deftest define-condition-slots-test
  (testing "define-condition preserves slot specifications"
    (let ((expansion (clysm::expand-define-condition
                      '(define-condition parse-error (error)
                        ((line :initarg :line :reader parse-error-line)
                         (column :initarg :column :reader parse-error-column))))))
      ;; Check slots are preserved
      (let ((slots (cadddr expansion)))
        (ok (= (length slots) 2)
            "Should have 2 slots")
        ;; First slot
        (let ((line-slot (find 'line slots :key #'car)))
          (ok line-slot "line slot should exist")
          (ok (getf (cdr line-slot) :initarg)
              "line slot should have :initarg"))
        ;; Second slot
        (let ((column-slot (find 'column slots :key #'car)))
          (ok column-slot "column slot should exist")
          (ok (getf (cdr column-slot) :reader)
              "column slot should have :reader"))))))

;;;; T031: define-condition with :report option
(deftest define-condition-report-option-test
  (testing "define-condition :report option is handled (skipped for basic support)"
    ;; Scenario 2 from spec.md:
    ;; Given: define-condition with :report option
    ;; Then: :report option is handled appropriately (registered or ignored)
    (let ((expansion (clysm::expand-define-condition
                      '(define-condition file-not-found (error)
                        ((path :initarg :path))
                        (:report (lambda (c s)
                                   (format s "File not found: ~A"
                                           (file-not-found-path c))))))))
      ;; Should still produce valid defclass
      (ok (eq (car expansion) 'defclass)
          "Should still expand to defclass")
      ;; :report should not appear in defclass options
      (let ((options (cddddr expansion)))
        (ok (not (member :report (mapcar #'car options)))
            ":report should not be in defclass options")))))

;;;; T032: nested condition inheritance
(deftest define-condition-inheritance-test
  (testing "nested condition inheritance compiles correctly"
    ;; Scenario 3 from spec.md:
    ;; Given: (define-condition parse-error (my-error) ...)
    ;; Then: both condition classes exist with correct inheritance
    (let ((base-expansion (clysm::expand-define-condition
                           '(define-condition my-error (error)
                             ((msg :initarg :msg)))))
          (derived-expansion (clysm::expand-define-condition
                              '(define-condition parse-error (my-error)
                                ((line :initarg :line))))))
      ;; Base class
      (ok (equal (caddr base-expansion) '(error))
          "my-error should inherit from error")
      ;; Derived class
      (ok (equal (caddr derived-expansion) '(my-error))
          "parse-error should inherit from my-error"))))
