;;; tests/unit/declare-skip-test.lisp - Unit tests for declare form handling
;;; Feature 038: Stage 0 Capability Extension
;;; TDD: Tests written FIRST, must FAIL before implementation

(in-package #:clysm/tests/unit/declare-skip)

;;;; Test Suite Definition
(deftest declare-skip-tests
  (testing "User Story 3: Handle Declaration Forms"
    ;; T040: filter-declare-forms function
    ;; T041: defun with single declare
    ;; T042: defun with multiple declare forms
    ;; T043: let with declare
    ;; T044: proclaim at top level
    ))

;;;; T040: filter-declare-forms function
(deftest filter-declare-forms-test
  (testing "filter-declare-forms should separate declarations from body"
    ;; Will fail until filter-declare-forms is implemented
    (ok (fboundp 'clysm::filter-declare-forms)
        "filter-declare-forms should be defined")

    ;; Test filtering
    (multiple-value-bind (body declarations)
        (clysm::filter-declare-forms
         '((declare (type fixnum x))
           (declare (optimize speed))
           (+ x 1)))
      (ok (= (length body) 1) "Body should have 1 form")
      (ok (equal (car body) '(+ x 1)) "Body should be (+ x 1)")
      (ok (= (length declarations) 2) "Should extract 2 declarations"))))

;;;; T041: defun with single declare
(deftest defun-single-declare-test
  (testing "defun with single declare compiles with declaration skipped"
    ;; Scenario 1 from spec.md:
    ;; Given: (defun foo (x) (declare (type fixnum x)) (+ x 1))
    ;; Then: declare is skipped and function body compiles correctly
    (let* ((form '(defun foo (x) (declare (type fixnum x)) (+ x 1)))
           (ast (clysm::parse-form form)))
      (ok (typep ast 'clysm::ast-defun)
          "Should parse as defun")
      ;; Body should not contain declare
      (let ((body (clysm::ast-defun-body ast)))
        (ok (not (and (consp (car body))
                      (eq (caar body) 'declare)))
            "Body should not start with declare")))))

;;;; T042: defun with multiple declare forms
(deftest defun-multiple-declare-test
  (testing "defun with multiple declare forms compiles correctly"
    ;; Scenario 2 from spec.md:
    ;; Given: (defun bar (x y) (declare (type integer x)) (declare (optimize speed)) ...)
    ;; Then: all declare forms are skipped and function compiles
    (let* ((form '(defun bar (x y)
                    (declare (type integer x))
                    (declare (optimize speed))
                    (+ x y)))
           (ast (clysm::parse-form form)))
      (ok (typep ast 'clysm::ast-defun)
          "Should parse as defun")
      ;; Body should only have the actual expression
      (let ((body (clysm::ast-defun-body ast)))
        (ok (= (length body) 1)
            "Body should have 1 form (declarations filtered)")
        (ok (equal (car body) '(+ x y))
            "Body should be (+ x y)")))))

;;;; T043: let with declare
(deftest let-with-declare-test
  (testing "let with declare compiles with declaration skipped"
    ;; Scenario 3 from spec.md:
    ;; Given: (let ((x 1)) (declare (type fixnum x)) x)
    ;; Then: declaration is skipped and let body compiles
    (let* ((form '(let ((x 1)) (declare (type fixnum x)) x))
           (ast (clysm::parse-form form)))
      (ok (typep ast 'clysm::ast-let)
          "Should parse as let")
      ;; Body should not contain declare
      (let ((body (clysm::ast-let-body ast)))
        (ok (not (and (consp (car body))
                      (eq (caar body) 'declare)))
            "Let body should not start with declare")))))

;;;; T044: proclaim at top level
(deftest proclaim-toplevel-skip-test
  (testing "proclaim at top level is skipped"
    ;; proclaim forms should be skipped without error
    (let ((form '(proclaim '(optimize (speed 3)))))
      ;; compilable-form-p should return NIL for proclaim
      (ok (not (clysm::compilable-form-p form))
          "proclaim should not be compilable (skipped)"))))
