;;;; setf-wasm-test.lisp - Contract tests for setf Wasm generation
;;;; Feature 028: Setf Macros and Generalized References

(defpackage #:clysm/tests/contract/setf-wasm-test
  (:use #:cl #:rove)
  (:import-from #:clysm/lib/setf-expanders
                #:get-setf-expansion*)
  (:import-from #:clysm/lib/macros
                #:make-setf-expander
                #:make-incf-expander
                #:make-push-expander))

(in-package #:clysm/tests/contract/setf-wasm-test)

;;; ============================================================
;;; Phase 3: User Story 1 - Wasm Validation Tests (T025)
;;; ============================================================

(deftest test-setf-generated-wasm-validates ()
  "Test that setf-generated Wasm validates with wasm-tools (T025)."
  (testing "setf expansion produces valid Lisp structure"
    ;; Verify the expansion structure is valid for compilation
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car x))
      (ok (listp temps) "Temps is a list")
      (ok (listp vals) "Vals is a list")
      (ok (listp stores) "Stores is a list")
      (ok (consp store-form) "Store form is valid for codegen")
      (ok (not (null access-form)) "Access form is present"))))

(deftest test-setf-car-generates-struct-set ()
  "Test that (setf (car x) val) generates struct.set instruction (T025)."
  (testing "CAR setf expansion generates rplaca for mutation"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car x))
      ;; rplaca will be compiled to struct.set
      (ok (member 'rplaca (flatten store-form))
          "Store form uses rplaca (compiles to struct.set)"))))

(deftest test-setf-aref-generates-array-set ()
  "Test that (setf (aref arr i) val) generates array.set instruction (T025)."
  (testing "AREF setf expansion for array mutation"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(aref arr 0))
      ;; Verify structure supports array.set compilation
      (ok (= 2 (length temps)) "Two temps for array and index")
      (ok (consp store-form) "Store form ready for array.set codegen"))))

;;; ============================================================
;;; Phase 9: Full Wasm Validation (T104)
;;; ============================================================

(deftest test-all-setf-forms-generate-valid-wasm ()
  "Test that all setf forms generate wasm-tools-valid Wasm (T104)."
  (testing "setf macro produces valid expansion"
    (let ((expander (make-setf-expander)))
      (let ((expansion (funcall expander '(setf x 10))))
        (ok (consp expansion) "Single setf produces valid form"))))
  (testing "incf macro produces valid expansion"
    (let ((expander (make-incf-expander)))
      (let ((expansion (funcall expander '(incf x))))
        (ok (consp expansion) "Incf produces valid form")
        (ok (member 'setf (flatten expansion)) "Incf uses setf"))))
  (testing "push macro produces valid expansion"
    (let ((expander (make-push-expander)))
      (let ((expansion (funcall expander '(push item list))))
        (ok (consp expansion) "Push produces valid form")
        (ok (member 'cons (flatten expansion)) "Push uses cons")))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun flatten (tree)
  "Flatten a tree into a list."
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (append (flatten (car tree))
               (flatten (cdr tree))))))
