;;;; special-var-test.lisp - Special variable integration tests
(in-package #:clysm/tests/integration/special-var)

;;; ============================================================
;;; Phase 3: User Story 1 - defvar/defparameter (T014-T016)
;;; ============================================================

;;; T014: Integration test for defvar basic usage

(deftest test-defvar-basic
  (testing "defvar creates accessible global variable"
    ;; (defvar *x* 10) *x* should return 10
    (ok (= 10 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defvar *test-x* 10)
                 *test-x*)))
        "defvar should create a global variable that can be accessed"))

  (testing "defvar with complex init-form"
    ;; (defvar *y* (+ 1 2 3)) *y* should return 6
    (ok (= 6 (clysm/tests/helpers:compile-and-run
              '(progn
                (defvar *test-y* (+ 1 2 3))
                *test-y*)))
        "defvar init-form should be evaluated")))

;;; T015: Integration test for defvar no-reinit semantics

(deftest test-defvar-no-reinit
  (testing "defvar does not reinitialize if already bound"
    ;; First defvar sets value, second defvar should NOT change it
    ;; (defvar *z* 1) (defvar *z* 2) *z* should still be 1
    (ok (= 1 (clysm/tests/helpers:compile-and-run
              '(progn
                (defvar *test-z* 1)
                (defvar *test-z* 2)
                *test-z*)))
        "Second defvar should not change existing value")))

;;; T016: Integration test for defparameter always-reinit semantics

(deftest test-defparameter-reinit
  (testing "defparameter always initializes"
    ;; (defparameter *p* 10) *p* should return 10
    (ok (= 10 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defparameter *test-p* 10)
                 *test-p*)))
        "defparameter should initialize variable"))

  (testing "defparameter always reinitializes"
    ;; Unlike defvar, defparameter always sets the value
    ;; (defparameter *q* 1) (defparameter *q* 2) *q* should be 2
    (ok (= 2 (clysm/tests/helpers:compile-and-run
              '(progn
                (defparameter *test-q* 1)
                (defparameter *test-q* 2)
                *test-q*)))
        "Second defparameter should update value")))

;;; ============================================================
;;; Phase 4: User Story 2 - Dynamic Binding (T026-T028)
;;; ============================================================

;;; T026: Integration test for let with single special binding

(deftest test-let-special-binding
  (testing "let binds special variable dynamically"
    ;; (defvar *x* 10) (let ((*x* 20)) *x*) should return 20
    (ok (= 20 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defvar *let-x* 10)
                 (let ((*let-x* 20))
                   *let-x*))))
        "let should dynamically bind special variable")))

;;; T027: Integration test for nested dynamic bindings

(deftest test-nested-dynamic-bindings
  (testing "nested let bindings work correctly"
    ;; (defvar *x* 1)
    ;; (let ((*x* 2))
    ;;   (let ((*x* 3))
    ;;     *x*))  ; should be 3
    (ok (= 3 (clysm/tests/helpers:compile-and-run
              '(progn
                (defvar *nest-x* 1)
                (let ((*nest-x* 2))
                  (let ((*nest-x* 3))
                    *nest-x*)))))
        "Innermost binding should be visible")))

;;; T028: Integration test for binding restoration after let

(deftest test-binding-restoration
  (testing "binding is restored after let exits"
    ;; (defvar *x* 10)
    ;; (progn
    ;;   (let ((*x* 20)) (setq dummy *x*))
    ;;   *x*)  ; should be 10 again
    (ok (= 10 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defvar *restore-x* 10)
                 (let ((*restore-x* 20))
                   *restore-x*)  ; This is 20
                 *restore-x*)))  ; Should be 10 again
        "Original value should be restored after let")))

;;; ============================================================
;;; Phase 5: User Story 3 - Lexical vs Special (T036, T039)
;;; ============================================================

;;; T036: Integration test for mixed lexical/special in same scope

(deftest test-mixed-lexical-special
  (testing "lexical and special variables coexist"
    ;; (defvar *s* 1)
    ;; (let ((l 2) (*s* 3))
    ;;   (+ l *s*))  ; should be 5
    (ok (= 5 (clysm/tests/helpers:compile-and-run
              '(progn
                (defvar *mix-s* 1)
                (let ((l 2) (*mix-s* 3))
                  (+ l *mix-s*)))))
        "Lexical and special should work in same let")))

;;; T039: Integration test for setq of special variable

(deftest test-setq-special
  (testing "setq updates special variable value"
    ;; (defvar *x* 1) (setq *x* 42) *x* should be 42
    (ok (= 42 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defvar *setq-x* 1)
                 (setq *setq-x* 42)
                 *setq-x*)))
        "setq should update special variable")))

;;; ============================================================
;;; Phase 6: User Story 4 - Gensym (T045-T047)
;;; ============================================================

;;; Placeholders for gensym tests (implemented in Phase 6)

(deftest test-gensym-basic
  (testing "gensym produces symbol"
    (ok t "Placeholder for gensym basic test")))

(deftest test-gensym-prefix
  (testing "gensym with custom prefix"
    (ok t "Placeholder for gensym prefix test")))

(deftest test-gensym-counter
  (testing "gensym increments counter"
    (ok t "Placeholder for gensym counter test")))

;;; ============================================================
;;; Phase 7: User Story 5 - Exception Safety (T052-T053)
;;; ============================================================

;;; Placeholders for exception safety tests

(deftest test-throw-catch-special
  (testing "throw/catch preserves special binding restoration"
    (ok t "Placeholder for throw/catch with special binding")))

(deftest test-return-from-special
  (testing "return-from preserves special binding restoration"
    (ok t "Placeholder for return-from with special binding")))

;;; ============================================================
;;; Shallow Binding Verification
;;; ============================================================

(deftest test-shallow-binding-semantics
  (testing "function sees dynamic binding from caller"
    ;; (defvar *x* 10)
    ;; (defun get-x () *x*)
    ;; (let ((*x* 20)) (get-x)) should return 20 (not 10)
    (ok (= 20 (clysm/tests/helpers:compile-and-run
               '(progn
                 (defvar *shallow-x* 10)
                 (defun get-shallow-x () *shallow-x*)
                 (let ((*shallow-x* 20))
                   (get-shallow-x)))))
        "Function should see caller's dynamic binding")))
