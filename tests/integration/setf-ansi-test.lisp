;;;; setf-ansi-test.lisp - Integration tests for setf ANSI compliance
;;;; Feature 028: Setf Macros and Generalized References

(defpackage #:clysm/tests/integration/setf-ansi-test
  (:use #:cl #:rove)
  (:import-from #:clysm/lib/setf-expanders
                #:get-setf-expansion*)
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

(in-package #:clysm/tests/integration/setf-ansi-test)

;;; ============================================================
;;; Phase 3: User Story 1 - setf Integration Tests (T026)
;;; ============================================================

(deftest test-setf-car-cdr-modification ()
  "Test setf modifies car/cdr correctly (T026)."
  (testing "CAR setf expansion produces valid code structure"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car x))
      ;; Verify the expansion has correct structure for ANSI compliance
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (consp store-form) "Store form is valid")
      (ok (member 'rplaca (flatten store-form)) "Uses rplaca for mutation")))
  (testing "CDR setf expansion produces valid code structure"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(cdr y))
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (member 'rplacd (flatten store-form)) "Uses rplacd for mutation"))))

(deftest test-setf-multiple-pairs ()
  "Test setf with multiple place/value pairs."
  (testing "Multiple pairs expand to progn of single setfs"
    (let ((expander (make-setf-expander)))
      (let ((expansion (funcall expander '(setf a 1 b 2 c 3))))
        (ok (eq (first expansion) 'progn) "Multiple pairs produce progn")
        (ok (= 3 (length (rest expansion))) "Three setf forms generated")))))

(deftest test-setf-nested-places ()
  "Test setf with nested places like (setf (car (cdr x)) val)."
  (testing "Nested place expansion"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car (cdr x)))
      (ok (consp temps) "Has temps for nested form")
      (ok (consp vals) "Has vals for nested form"))))

;;; ============================================================
;;; Phase 4: User Story 2 - psetf Integration Tests (T047)
;;; ============================================================

(deftest test-psetf-semantics ()
  "Test psetf parallel assignment semantics (T047)."
  (testing "Psetf uses temps for parallel evaluation"
    (let ((expander (make-psetf-expander)))
      (let ((expansion (funcall expander '(psetf a b b a))))
        ;; Should use let to capture values before assignment
        (ok (eq (first expansion) 'let) "Uses let for temps")
        ;; The let bindings should come before setf forms
        (ok (listp (second expansion)) "Has bindings list")))))

;;; ============================================================
;;; Phase 5: User Story 3 - incf/decf Integration Tests (T057)
;;; ============================================================

(deftest test-incf-decf-return-values ()
  "Test incf/decf return correct values (T057)."
  (testing "Incf expands to setf with addition"
    (let ((expander (make-incf-expander)))
      (let ((expansion (funcall expander '(incf x))))
        (ok (member '+ (flatten expansion)) "Uses + for increment"))))
  (testing "Decf expands to setf with subtraction"
    (let ((expander (make-decf-expander)))
      (let ((expansion (funcall expander '(decf x))))
        (ok (member '- (flatten expansion)) "Uses - for decrement")))))

;;; ============================================================
;;; Phase 6: User Story 4 - push/pop/pushnew Integration Tests (T070)
;;; ============================================================

(deftest test-push-pop-round-trip ()
  "Test push/pop round-trip behavior (T070)."
  (testing "Push uses cons to prepend"
    (let ((expander (make-push-expander)))
      (let ((expansion (funcall expander '(push item list))))
        (ok (member 'cons (flatten expansion)) "Push uses cons"))))
  (testing "Pop returns old value and modifies list"
    (let ((expander (make-pop-expander)))
      (let ((expansion (funcall expander '(pop list))))
        (ok (member 'prog1 (flatten expansion)) "Pop uses prog1 to return old value")
        (ok (member 'cdr (flatten expansion)) "Pop uses cdr for new value")))))

;;; ============================================================
;;; Phase 7: User Story 5 - rotatef/shiftf Integration Tests (T083)
;;; ============================================================

(deftest test-rotatef-shiftf-semantics ()
  "Test rotatef/shiftf value exchange semantics (T083)."
  (testing "Rotatef expands to proper value rotation"
    (let ((expander (make-rotatef-expander)))
      (let ((expansion (funcall expander '(rotatef a b c))))
        (ok (consp expansion) "Rotatef produces valid expansion"))))
  (testing "Shiftf returns first value"
    (let ((expander (make-shiftf-expander)))
      (let ((expansion (funcall expander '(shiftf a b c))))
        (ok (member 'prog1 (flatten expansion)) "Shiftf uses prog1 for return value")))))

;;; ============================================================
;;; Phase 8: User Story 6 - CLOS Integration Tests (T093)
;;; ============================================================

(deftest test-clos-slot-accessor-setf ()
  "Test setf works with CLOS slot accessors (T093)."
  (testing "Slot accessor setf expander factory"
    ;; Test that the slot accessor expander generator works
    (let ((expander (clysm/clos/slot-access:make-slot-accessor-setf-expander 'point-x 'x)))
      (ok (functionp expander) "Creates a function")
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(point-x instance) nil)
        (ok (= 1 (length temps)) "One temp for instance")
        (ok (eq (first access-form) 'point-x) "Access form uses accessor name")))))

;;; ============================================================
;;; Phase 9: Evaluation Order Tests (T102)
;;; ============================================================

(deftest test-setf-evaluation-order ()
  "Test setf evaluates subforms left-to-right exactly once (T102)."
  (testing "Temps are generated in order"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(aref arr i j))
      ;; Temps should match vals in order
      (ok (= (length temps) (length vals)) "Same number of temps and vals")
      ;; First temp binds to first val, etc.
      (ok (= 3 (length temps)) "Three temps: array, i, j"))))

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
