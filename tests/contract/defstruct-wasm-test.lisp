;;; defstruct-wasm-test.lisp - Contract tests for defstruct Wasm output
;;; Phase 13D-10: DEFSTRUCT Wasm Compilation
;;;
;;; Validates defstruct macro expansion and Wasm compilation.
;;; Full Wasm output validation requires CLOS runtime support.
;;;
;;; Reference: [defstruct](resources/HyperSpec/Body/m_defstr.htm)

(in-package :clysm/tests)

;;;; ============================================================
;;;; T020: DEFSTRUCT Macro Expansion Validation
;;;; ============================================================

;;; Note: Full Wasm compilation of defstruct requires CLOS runtime functions
;;; (make-instance*, slot-value*, etc.) to be compilable. Until then, we test:
;;; 1. Macro expansion structure is correct
;;; 2. Generated forms have expected shape

(deftest defstruct-basic-wasm-validate-test
  (testing "T020: basic defstruct expansion structure"
    ;; Verify macro expansion produces correct structure
    (let* ((form '(defstruct point x y))
           (def (clysm/lib::parse-defstruct form))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (eq 'progn (first expansion))
          "defstruct expands to progn")
      (ok (>= (length (rest expansion)) 5)
          "expansion contains at least 5 forms (class, constructor, accessors, predicate, copier)"))))

(deftest defstruct-constructor-expansion-test
  (testing "T020: defstruct generates constructor function"
    ;; Verify constructor is in expansion
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'make-point (second form))))
                (rest expansion))
          "expansion includes make-point constructor"))))

(deftest defstruct-accessor-expansion-test
  (testing "T020: defstruct generates accessor functions"
    ;; Verify accessors are in expansion
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'point-x (second form))))
                (rest expansion))
          "expansion includes point-x accessor")
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'point-y (second form))))
                (rest expansion))
          "expansion includes point-y accessor"))))

(deftest defstruct-predicate-expansion-test
  (testing "T020: defstruct generates predicate function"
    ;; Verify predicate is in expansion
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'point-p (second form))))
                (rest expansion))
          "expansion includes point-p predicate"))))

(deftest defstruct-copier-expansion-test
  (testing "T020: defstruct generates copier function"
    ;; Verify copier is in expansion
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'copy-point (second form))))
                (rest expansion))
          "expansion includes copy-point copier"))))

(deftest defstruct-setf-expansion-test
  (testing "T020: defstruct generates setf expanders"
    ;; Verify setf expander registrations are in expansion
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'clysm/lib/setf-expanders:register-setf-expander
                           (first form))))
                (rest expansion))
          "expansion includes setf expander registration"))))

;;;; ============================================================
;;;; Contract Tests for Options (Phase 4)
;;;; ============================================================

(deftest defstruct-conc-name-expansion-test
  (testing "defstruct :conc-name option generates correct accessor names"
    ;; Verify :conc-name option is respected
    (let* ((def (clysm/lib::parse-defstruct
                 '(defstruct (node (:conc-name n-)) left right)))
           (expansion (clysm/lib::expand-defstruct def)))
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'n-left (second form))))
                (rest expansion))
          "expansion includes n-left accessor (not node-left)")
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'n-right (second form))))
                (rest expansion))
          "expansion includes n-right accessor"))))

(deftest defstruct-slot-initform-expansion-test
  (testing "defstruct slot initform is preserved in expansion"
    ;; Verify initform is in constructor
    (let* ((def (clysm/lib::parse-defstruct '(defstruct counter (value 0))))
           (expansion (clysm/lib::expand-defstruct def)))
      ;; Find the constructor
      (let ((ctor (find-if (lambda (form)
                             (and (listp form)
                                  (eq 'defun (first form))
                                  (eq 'make-counter (second form))))
                           (rest expansion))))
        (ok ctor "expansion includes make-counter constructor")
        ;; The constructor should have (value 0) as default
        (when ctor
          (ok (member '(value 0) (third ctor) :test #'equal)
              "constructor has (value 0) default parameter"))))))

;;;; ============================================================
;;;; Expansion Structure Validation
;;;; ============================================================

(deftest defstruct-expansion-structure-test
  (testing "defstruct expansion has valid structure"
    (let* ((def (clysm/lib::parse-defstruct '(defstruct point x y)))
           (expansion (clysm/lib::expand-defstruct def)))
      ;; Verify expansion is a progn
      (ok (eq 'progn (first expansion))
          "Expansion is a progn form")
      ;; Verify structure class registration
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'clysm/clos/mop:register-structure-class (first form))))
                (rest expansion))
          "Expansion includes structure class registration")
      ;; Verify defclass form
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'clysm/clos/defclass:define-class* (first form))))
                (rest expansion))
          "Expansion includes define-class* form")
      ;; Verify constructor defun
      (ok (some (lambda (form)
                  (and (listp form)
                       (eq 'defun (first form))
                       (eq 'make-point (second form))))
                (rest expansion))
          "Expansion includes make-point constructor"))))
