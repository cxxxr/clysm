;;;; tests/e2e/macro-test.lisp - E2E Tests for Macros
;;;;
;;;; Tests that DEFMACRO and macro expansion work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-macro-suite "E2E tests for macro expansion")

;;; ============================================================
;;; Basic DEFMACRO Tests
;;; ============================================================

(deftest test-e2e-defmacro-simple ()
  "Simple macro"
  (is-e2e-forms "(defmacro my-quote (x) `(quote ,x))
                 (my-quote foo)"
                "FOO"))

(deftest test-e2e-defmacro-when ()
  "WHEN macro"
  (is-e2e-forms "(defmacro my-when (test &body body)
                   `(if ,test (progn ,@body)))
                 (my-when t 1 2 3)"
                "3"))

(deftest test-e2e-defmacro-when-false ()
  "WHEN macro with false condition"
  (is-e2e-forms "(defmacro my-when (test &body body)
                   `(if ,test (progn ,@body)))
                 (my-when nil 1 2 3)"
                "NIL"))

(deftest test-e2e-defmacro-unless ()
  "UNLESS macro"
  (is-e2e-forms "(defmacro my-unless (test &body body)
                   `(if ,test nil (progn ,@body)))
                 (my-unless nil 42)"
                "42"))

;;; ============================================================
;;; Macro with Computed Values
;;; ============================================================

(deftest test-e2e-defmacro-inc ()
  "INC macro"
  (is-e2e-forms "(defmacro inc (x) `(+ ,x 1))
                 (inc 5)"
                "6"))

(deftest test-e2e-defmacro-dec ()
  "DEC macro"
  (is-e2e-forms "(defmacro dec (x) `(- ,x 1))
                 (dec 5)"
                "4"))

(deftest test-e2e-defmacro-square ()
  "SQUARE macro"
  (is-e2e-forms "(defmacro square (x) `(* ,x ,x))
                 (square 5)"
                "25"))

;;; ============================================================
;;; Nested Macro Calls
;;; ============================================================

(deftest test-e2e-defmacro-nested ()
  "Nested macro calls"
  (is-e2e-forms "(defmacro inc (x) `(+ ,x 1))
                 (inc (inc (inc 0)))"
                "3"))

(deftest test-e2e-defmacro-nested-different ()
  "Nested different macros"
  (is-e2e-forms "(defmacro double (x) `(* ,x 2))
                 (defmacro inc (x) `(+ ,x 1))
                 (double (inc 5))"
                "12"))

;;; ============================================================
;;; Macro with Multiple Arguments
;;; ============================================================

(deftest test-e2e-defmacro-swap ()
  "SWAP macro (conceptual test)"
  (is-e2e-forms "(defmacro my-if2 (test then else)
                   `(if ,test ,then ,else))
                 (my-if2 t 'yes 'no)"
                "YES"))

;;; ============================================================
;;; Backquote Patterns
;;; ============================================================

(deftest test-e2e-backquote-simple ()
  "Simple backquote"
  (is-e2e-forms "(defmacro make-list-3 (a b c)
                   `(quote (,a ,b ,c)))
                 (make-list-3 x y z)"
                "(X Y Z)"))

(deftest test-e2e-backquote-splice ()
  "Backquote with splice"
  (is-e2e-forms "(defmacro with-body (&body forms)
                   `(progn ,@forms))
                 (with-body 1 2 3)"
                "3"))

;;; ============================================================
;;; Macro Defining Functions
;;; ============================================================

(deftest test-e2e-defmacro-defun-wrapper ()
  "Macro that wraps defun"
  (is-e2e-forms "(defmacro define-doubler (name)
                   `(defun ,name (x) (* x 2)))
                 (define-doubler my-double)
                 (my-double 21)"
                "42"))

;;; ============================================================
;;; Complex Macro Patterns
;;; ============================================================

(deftest test-e2e-defmacro-and-pattern ()
  "AND-like macro (short-circuit)"
  (is-e2e-forms "(defmacro my-and (a b)
                   `(if ,a ,b nil))
                 (my-and t 42)"
                "42"))

(deftest test-e2e-defmacro-or-pattern ()
  "OR-like macro"
  (is-e2e-forms "(defmacro my-or (a b)
                   `(if ,a ,a ,b))
                 (my-or nil 42)"
                "42"))
