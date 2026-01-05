;;;; tests/e2e/special-var-test.lisp - E2E Tests for Special Variables
;;;;
;;;; Tests that dynamic scoping works correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-special-var-suite "E2E tests for special variables (dynamic scoping)")

;;; ============================================================
;;; Basic DEFVAR Tests
;;; ============================================================

(deftest test-e2e-defvar-simple ()
  "Simple defvar"
  (is-e2e-forms "(defvar *x* 10)
                 *x*"
                "10"))

(deftest test-e2e-defvar-nil ()
  "Defvar with NIL"
  (is-e2e-forms "(defvar *empty* nil)
                 *empty*"
                "NIL"))

(deftest test-e2e-defvar-symbol ()
  "Defvar with symbol value"
  (is-e2e-forms "(defvar *mode* 'normal)
                 *mode*"
                "NORMAL"))

;;; ============================================================
;;; DEFPARAMETER Tests
;;; ============================================================

(deftest test-e2e-defparameter ()
  "Defparameter"
  (is-e2e-forms "(defparameter *param* 42)
                 *param*"
                "42"))

;;; ============================================================
;;; SETQ on Special Variables
;;; ============================================================

(deftest test-e2e-special-setq ()
  "Setq on special variable"
  (is-e2e-forms "(defvar *counter* 0)
                 (setq *counter* 10)
                 *counter*"
                "10"))

(deftest test-e2e-special-increment ()
  "Incrementing special variable"
  (is-e2e-forms "(defvar *n* 5)
                 (setq *n* (+ *n* 1))
                 *n*"
                "6"))

;;; ============================================================
;;; Dynamic Binding Tests
;;; ============================================================

(deftest test-e2e-dynamic-binding ()
  "Dynamic binding shadows global"
  (is-e2e-forms "(defvar *dyn* 'outer)
                 (defun get-dyn () *dyn*)
                 (let ((*dyn* 'inner))
                   (get-dyn))"
                "INNER"))

(deftest test-e2e-dynamic-restore ()
  "Dynamic binding restores on exit"
  (is-e2e-forms "(defvar *temp* 'original)
                 (let ((*temp* 'temporary))
                   1)
                 *temp*"
                "ORIGINAL"))

(deftest test-e2e-dynamic-nested ()
  "Nested dynamic bindings"
  (is-e2e-forms "(defvar *level* 0)
                 (defun get-level () *level*)
                 (let ((*level* 1))
                   (let ((*level* 2))
                     (get-level)))"
                "2"))

;;; ============================================================
;;; Dynamic Variables in Functions
;;; ============================================================

(deftest test-e2e-special-function-access ()
  "Function accessing special variable"
  (is-e2e-forms "(defvar *config* 'default)
                 (defun get-config () *config*)
                 (get-config)"
                "DEFAULT"))

(deftest test-e2e-special-function-modify ()
  "Function modifying special variable"
  (is-e2e-forms "(defvar *state* 0)
                 (defun inc-state ()
                   (setq *state* (+ *state* 1))
                   *state*)
                 (inc-state)
                 (inc-state)
                 *state*"
                "2"))

;;; ============================================================
;;; Dynamic Scope vs Lexical Scope
;;; ============================================================

(deftest test-e2e-dynamic-vs-lexical ()
  "Dynamic scope affects called functions"
  (is-e2e-forms "(defvar *dynamic* 'global)
                 (defun reader () *dynamic*)
                 (defun caller ()
                   (let ((*dynamic* 'local))
                     (reader)))
                 (caller)"
                "LOCAL"))

(deftest test-e2e-special-in-closure ()
  "Special variable access from closure"
  (is-e2e-forms "(defvar *base* 100)
                 (defun make-adder ()
                   (lambda (x) (+ x *base*)))
                 (let ((*base* 200))
                   ((make-adder) 5))"
                "205"))

;;; ============================================================
;;; Multiple Special Variables
;;; ============================================================

(deftest test-e2e-multiple-specials ()
  "Multiple special variables"
  (is-e2e-forms "(defvar *a* 1)
                 (defvar *b* 2)
                 (defvar *c* 3)
                 (+ *a* (+ *b* *c*))"
                "6"))

(deftest test-e2e-multiple-dynamic-bindings ()
  "Binding multiple specials at once"
  (is-e2e-forms "(defvar *x* 0)
                 (defvar *y* 0)
                 (let ((*x* 10) (*y* 20))
                   (+ *x* *y*))"
                "30"))
