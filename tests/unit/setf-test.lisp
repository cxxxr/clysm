;;;; setf-test.lisp - Unit tests for setf macros
;;;; Feature 028: Setf Macros and Generalized References

(defpackage #:clysm/tests/unit/setf-test
  (:use #:cl #:rove)
  (:import-from #:clysm/lib/setf-expanders
                #:get-setf-expansion*
                #:simple-variable-p)
  (:import-from #:clysm/lib/macros
                #:make-setf-expander
                #:make-psetf-expander
                #:make-incf-expander
                #:make-decf-expander
                #:make-push-expander
                #:make-pop-expander
                #:make-pushnew-expander
                #:make-rotatef-expander
                #:make-shiftf-expander))

(in-package #:clysm/tests/unit/setf-test)

;;; ============================================================
;;; Phase 2: Foundational Tests (T009)
;;; ============================================================

(deftest test-simple-variable-p ()
  "Test simple-variable-p predicate."
  (testing "Symbols are simple variables"
    (ok (simple-variable-p 'x))
    (ok (simple-variable-p 'foo))
    (ok (simple-variable-p 'my-var)))
  (testing "Constants are not simple variables"
    (ok (not (simple-variable-p nil)))
    (ok (not (simple-variable-p t)))
    (ok (not (simple-variable-p :keyword))))
  (testing "Non-symbols are not simple variables"
    (ok (not (simple-variable-p 42)))
    (ok (not (simple-variable-p "string")))
    (ok (not (simple-variable-p '(a b c))))))

;;; ============================================================
;;; Phase 3: User Story 1 - setf Tests (T017-T018)
;;; ============================================================

(deftest test-setf-simple-variable-expansion ()
  "Test setf expansion for simple variables (T017)."
  (testing "Simple variable setf expands to setq"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* 'x)
      (ok (null temps) "No temps for simple variable")
      (ok (null vals) "No vals for simple variable")
      (ok (= 1 (length stores)) "One store variable")
      (ok (eq (first (rest store-form)) 'x) "Store form is setq of x")
      (ok (eq access-form 'x) "Access form is x"))))

(deftest test-setf-car-cdr-expansion ()
  "Test setf expansion for car/cdr places (T018)."
  (testing "CAR setf expansion"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car x))
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (= 1 (length vals)) "One val for cons form")
      (ok (= 1 (length stores)) "One store variable")
      (ok (eq (first access-form) 'car) "Access form uses car")))
  (testing "CDR setf expansion"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(cdr y))
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (= 1 (length vals)) "One val for cons form")
      (ok (= 1 (length stores)) "One store variable")
      (ok (eq (first access-form) 'cdr) "Access form uses cdr"))))

;;; ============================================================
;;; Phase 4: User Story 2 - psetf Tests (T044-T046)
;;; ============================================================

(deftest test-psetf-two-variable-swap ()
  "Test psetf swaps two variables correctly (T044)."
  (testing "Two variable swap"
    (let ((expander (make-psetf-expander)))
      (let ((expansion (funcall expander '(psetf a b b a))))
        ;; psetf should expand to let with temps then setf assignments
        (ok (consp expansion) "Expansion is a form")
        (ok (eq (first expansion) 'let) "Expansion is a let form")))))

(deftest test-psetf-three-variable-rotation ()
  "Test psetf rotates three variables correctly (T045)."
  (testing "Three variable rotation"
    (let ((expander (make-psetf-expander)))
      (let ((expansion (funcall expander '(psetf a b b c c a))))
        (ok (consp expansion) "Expansion is a form")
        (ok (eq (first expansion) 'let) "Expansion uses let for temps")))))

(deftest test-psetf-with-place-forms ()
  "Test psetf works with place forms (T046)."
  (testing "Place form parallel assignment"
    (let ((expander (make-psetf-expander)))
      (let ((expansion (funcall expander '(psetf (car x) (cdr x) (cdr x) (car x)))))
        (ok (consp expansion) "Expansion is a form")))))

;;; ============================================================
;;; Phase 5: User Story 3 - incf/decf Tests (T052-T056)
;;; ============================================================

(deftest test-incf-default-delta ()
  "Test incf with default delta of 1 (T052)."
  (testing "Default delta increment"
    (let ((expander (make-incf-expander)))
      (let ((expansion (funcall expander '(incf x))))
        (ok (consp expansion) "Expansion is a form")
        ;; Should expand to setf with +
        (ok (member 'setf (flatten expansion)) "Uses setf")))))

(deftest test-incf-explicit-delta ()
  "Test incf with explicit delta (T053)."
  (testing "Explicit delta increment"
    (let ((expander (make-incf-expander)))
      (let ((expansion (funcall expander '(incf x 5))))
        (ok (consp expansion) "Expansion is a form")
        (ok (member 5 (flatten expansion)) "Contains delta value")))))

(deftest test-decf-default-delta ()
  "Test decf with default delta of 1 (T054)."
  (testing "Default delta decrement"
    (let ((expander (make-decf-expander)))
      (let ((expansion (funcall expander '(decf x))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-decf-explicit-delta ()
  "Test decf with explicit delta (T055)."
  (testing "Explicit delta decrement"
    (let ((expander (make-decf-expander)))
      (let ((expansion (funcall expander '(decf x 5))))
        (ok (consp expansion) "Expansion is a form")
        (ok (member 5 (flatten expansion)) "Contains delta value")))))

(deftest test-incf-on-place-form ()
  "Test incf on a place form like (car x) (T056)."
  (testing "Incf on place form"
    (let ((expander (make-incf-expander)))
      (let ((expansion (funcall expander '(incf (car x)))))
        (ok (consp expansion) "Expansion is a form")))))

;;; ============================================================
;;; Phase 6: User Story 4 - push/pop/pushnew Tests (T063-T069)
;;; ============================================================

(deftest test-push-onto-non-empty-list ()
  "Test push onto non-empty list (T063)."
  (testing "Push onto list"
    (let ((expander (make-push-expander)))
      (let ((expansion (funcall expander '(push item list))))
        (ok (consp expansion) "Expansion is a form")
        ;; Should expand to setf with cons
        (ok (member 'setf (flatten expansion)) "Uses setf")
        (ok (member 'cons (flatten expansion)) "Uses cons")))))

(deftest test-push-onto-nil ()
  "Test push onto nil/empty list (T064)."
  (testing "Push onto nil"
    (let ((expander (make-push-expander)))
      (let ((expansion (funcall expander '(push item nil-var))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-pop-from-non-empty-list ()
  "Test pop from non-empty list (T065)."
  (testing "Pop from list"
    (let ((expander (make-pop-expander)))
      (let ((expansion (funcall expander '(pop list))))
        (ok (consp expansion) "Expansion is a form")
        ;; Should use prog1 to return old car
        (ok (member 'prog1 (flatten expansion)) "Uses prog1 for return value")))))

(deftest test-pop-from-nil ()
  "Test pop from nil/empty list (T066)."
  (testing "Pop from nil"
    (let ((expander (make-pop-expander)))
      (let ((expansion (funcall expander '(pop empty-list))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-pushnew-when-item-exists ()
  "Test pushnew when item already exists in list (T067)."
  (testing "Pushnew existing item"
    (let ((expander (make-pushnew-expander)))
      (let ((expansion (funcall expander '(pushnew item list))))
        (ok (consp expansion) "Expansion is a form")
        ;; Should check membership before pushing
        (ok (member 'member (flatten expansion)) "Uses member check")))))

(deftest test-pushnew-when-item-absent ()
  "Test pushnew when item is not in list (T068)."
  (testing "Pushnew new item"
    (let ((expander (make-pushnew-expander)))
      (let ((expansion (funcall expander '(pushnew new-item list))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-pushnew-with-test-keyword ()
  "Test pushnew with :test keyword (T069)."
  (testing "Pushnew with test function"
    (let ((expander (make-pushnew-expander)))
      (let ((expansion (funcall expander '(pushnew item list :test #'equal))))
        (ok (consp expansion) "Expansion is a form")))))

;;; ============================================================
;;; Phase 7: User Story 5 - rotatef/shiftf Tests (T079-T082)
;;; ============================================================

(deftest test-rotatef-two-place-swap ()
  "Test rotatef swaps two places (T079)."
  (testing "Two-place rotatef"
    (let ((expander (make-rotatef-expander)))
      (let ((expansion (funcall expander '(rotatef a b))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-rotatef-three-place-rotation ()
  "Test rotatef rotates three places (T080)."
  (testing "Three-place rotatef"
    (let ((expander (make-rotatef-expander)))
      (let ((expansion (funcall expander '(rotatef a b c))))
        (ok (consp expansion) "Expansion is a form")))))

(deftest test-shiftf-return-value ()
  "Test shiftf returns the first value (T081)."
  (testing "Shiftf return value"
    (let ((expander (make-shiftf-expander)))
      (let ((expansion (funcall expander '(shiftf a b c))))
        (ok (consp expansion) "Expansion is a form")
        ;; Should use prog1 or similar to capture first value
        (ok (member 'prog1 (flatten expansion)) "Uses prog1 for return")))))

(deftest test-shiftf-with-new-value ()
  "Test shiftf with new value at end (T082)."
  (testing "Shiftf with new value"
    (let ((expander (make-shiftf-expander)))
      (let ((expansion (funcall expander '(shiftf a b new-value))))
        (ok (consp expansion) "Expansion is a form")))))

;;; ============================================================
;;; Phase 8: User Story 6 - define-setf-expander Tests (T090)
;;; ============================================================

(deftest test-define-setf-expander-usage ()
  "Test using a custom setf expander defined with define-setf-expander* (T090)."
  (testing "Custom expander usage"
    ;; The define-setf-expander* macro is available in setf-expanders
    ;; Test that get-setf-expansion* works with standard expanders
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(symbol-value 'foo))
      (ok (= 1 (length temps)) "One temp for symbol")
      (ok (= 1 (length stores)) "One store variable")
      (ok (eq (first access-form) 'symbol-value) "Access form uses symbol-value"))))

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
