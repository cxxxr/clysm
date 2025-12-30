;;; defstruct-usage-test.lisp - Integration tests for defstruct
;;; Phase 13D-10: DEFSTRUCT Wasm Compilation
;;;
;;; Tests defstruct macro integration with the compiler pipeline.
;;; Full Wasm compilation requires CLOS runtime support (Phase 4+).
;;;
;;; Reference: [defstruct](resources/HyperSpec/Body/m_defstr.htm)

(in-package :clysm/tests)

;;;; ============================================================
;;;; T021: Defstruct Integration Tests
;;;; ============================================================

;;; Note: Full Wasm compilation of defstruct-using code requires CLOS runtime
;;; functions to be compilable. These tests verify:
;;; 1. Macro registration works correctly
;;; 2. Expansion integrates with compiler pipeline
;;; 3. Generated function signatures are correct

(deftest defstruct-macro-registration-test
  (testing "T021: defstruct macro is registered in global registry"
    (let ((expander (clysm/compiler/transform/macro:macro-function*
                     (clysm/compiler/transform/macro:global-macro-registry)
                     'defstruct)))
      (ok expander "defstruct macro is registered")
      (ok (functionp expander) "defstruct expander is a function"))))

(deftest defstruct-expansion-integration-test
  (testing "T021: defstruct expansion produces compilable defuns"
    ;; Verify each generated defun has correct structure
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      ;; Collect all defuns
      (let ((defuns (remove-if-not
                     (lambda (form) (and (listp form) (eq 'defun (first form))))
                     (rest expansion))))
        (ok (>= (length defuns) 4)
            "expansion generates at least 4 functions (constructor, 2 accessors, predicate, copier)")
        ;; Each defun should have: (defun name lambda-list body...)
        (dolist (defun-form defuns)
          (ok (>= (length defun-form) 3)
              "each defun has at least name and lambda-list"))))))

(deftest defstruct-constructor-signature-test
  (testing "T021: generated constructor has keyword parameters"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (let ((ctor (find-if (lambda (f)
                             (and (listp f) (eq 'defun (first f))
                                  (eq 'make-point (second f))))
                           (rest expansion))))
        (ok ctor "make-point constructor generated")
        (when ctor
          (let ((lambda-list (third ctor)))
            (ok (eq '&key (first lambda-list))
                "constructor uses &key parameters")))))))

(deftest defstruct-accessor-signature-test
  (testing "T021: generated accessors take single instance parameter"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (let ((accessor (find-if (lambda (f)
                                 (and (listp f) (eq 'defun (first f))
                                      (eq 'point-x (second f))))
                               (rest expansion))))
        (ok accessor "point-x accessor generated")
        (when accessor
          (let ((lambda-list (third accessor)))
            (ok (= 1 (length lambda-list))
                "accessor takes exactly one parameter")))))))

(deftest defstruct-predicate-signature-test
  (testing "T021: generated predicate takes single object parameter"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (let ((pred (find-if (lambda (f)
                             (and (listp f) (eq 'defun (first f))
                                  (eq 'point-p (second f))))
                           (rest expansion))))
        (ok pred "point-p predicate generated")
        (when pred
          (let ((lambda-list (third pred)))
            (ok (= 1 (length lambda-list))
                "predicate takes exactly one parameter")))))))

(deftest defstruct-copier-signature-test
  (testing "T021: generated copier takes single instance parameter"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (let ((copier (find-if (lambda (f)
                               (and (listp f) (eq 'defun (first f))
                                    (eq 'copy-point (second f))))
                             (rest expansion))))
        (ok copier "copy-point copier generated")
        (when copier
          (let ((lambda-list (third copier)))
            (ok (= 1 (length lambda-list))
                "copier takes exactly one parameter")))))))

(deftest defstruct-setf-expander-registration-test
  (testing "T021: defstruct registers setf expanders for accessors"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      ;; Count setf expander registrations
      (let ((setf-forms (remove-if-not
                         (lambda (f)
                           (and (listp f)
                                (eq 'clysm/lib/setf-expanders:register-setf-expander
                                    (first f))))
                         (rest expansion))))
        (ok (= 2 (length setf-forms))
            "two setf expanders registered (for x and y slots)")))))
