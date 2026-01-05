;;;; tests/e2e/vector-test.lisp - E2E Tests for Vector Operations
;;;;
;;;; Tests that vector primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-vector-suite "E2E tests for vector operations")

;;; ============================================================
;;; Basic Vector Creation
;;; ============================================================

(deftest test-e2e-make-vector ()
  "Create vector with make-vector"
  (e2e-finishes "(make-vector 5 nil)"))

(deftest test-e2e-make-vector-zero ()
  "Create vector initialized with zeros"
  (e2e-finishes "(make-vector 3 0)"))

;;; ============================================================
;;; Vector Length
;;; ============================================================

(deftest test-e2e-vector-length ()
  "Get vector length"
  (is-e2e "(vector-length (make-vector 5 nil))" "5"))

(deftest test-e2e-vector-length-various ()
  "Vector lengths"
  (is-e2e "(vector-length (make-vector 0 nil))" "0")
  (is-e2e "(vector-length (make-vector 10 nil))" "10"))

;;; ============================================================
;;; Vector Access (svref)
;;; ============================================================

(deftest test-e2e-svref-initial ()
  "Access initial vector elements"
  (is-e2e "(let ((v (make-vector 3 0)))
            (svref v 0))"
          "0"))

(deftest test-e2e-svref-nil-init ()
  "Access nil-initialized elements"
  (is-e2e "(let ((v (make-vector 3 nil)))
            (svref v 1))"
          "NIL"))

;;; ============================================================
;;; Vector Modification (svset)
;;; ============================================================

(deftest test-e2e-svset ()
  "Set vector element"
  (is-e2e "(let ((v (make-vector 3 0)))
            (svset v 0 42)
            (svref v 0))"
          "42"))

(deftest test-e2e-svset-multiple ()
  "Set multiple vector elements"
  (is-e2e "(let ((v (make-vector 3 0)))
            (svset v 0 10)
            (svset v 1 20)
            (svset v 2 30)
            (+ (svref v 0) (+ (svref v 1) (svref v 2))))"
          "60"))

;;; ============================================================
;;; Vector with Different Types
;;; ============================================================

(deftest test-e2e-vector-symbols ()
  "Vector containing symbols"
  (is-e2e "(let ((v (make-vector 2 nil)))
            (svset v 0 'foo)
            (svset v 1 'bar)
            (svref v 0))"
          "FOO"))

(deftest test-e2e-vector-cons ()
  "Vector containing cons cells"
  (is-e2e "(let ((v (make-vector 1 nil)))
            (svset v 0 (cons 1 2))
            (car (svref v 0)))"
          "1"))

;;; ============================================================
;;; Vector in Functions
;;; ============================================================

(deftest test-e2e-vector-sum ()
  "Sum elements of a vector"
  (is-e2e-forms "(defun vector-sum (v len)
                   (let ((sum 0) (i 0))
                     (tagbody
                       loop
                       (if (= i len) (go done))
                       (setq sum (+ sum (svref v i)))
                       (setq i (+ i 1))
                       (go loop)
                       done)
                     sum))
                 (let ((v (make-vector 3 0)))
                   (svset v 0 10)
                   (svset v 1 20)
                   (svset v 2 30)
                   (vector-sum v 3))"
                "60"))

;;; ============================================================
;;; VECTORP Predicate
;;; ============================================================

(deftest test-e2e-vectorp-true ()
  "VECTORP on vectors"
  (is-e2e "(vectorp (make-vector 3 nil))" "T"))

(deftest test-e2e-vectorp-false ()
  "VECTORP on non-vectors"
  (is-e2e "(vectorp '(1 2 3))" "NIL")
  (is-e2e "(vectorp 42)" "NIL")
  (is-e2e "(vectorp 'foo)" "NIL"))
