;;;; tests/e2e/predicates-test.lisp - E2E Tests for Type Predicates
;;;;
;;;; Tests that type predicate primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-predicates-suite "E2E tests for type predicates")

;;; ============================================================
;;; SYMBOLP Tests
;;; ============================================================

(deftest test-e2e-symbolp-true ()
  "SYMBOLP - true cases"
  (is-e2e "(symbolp 'foo)" "T")
  (is-e2e "(symbolp 'bar)" "T")
  (is-e2e "(symbolp t)" "T")
  (is-e2e "(symbolp nil)" "T"))

(deftest test-e2e-symbolp-false ()
  "SYMBOLP - false cases"
  (is-e2e "(symbolp 42)" "NIL")
  (is-e2e "(symbolp (cons 1 2))" "NIL"))

;;; ============================================================
;;; NUMBERP Tests
;;; ============================================================

(deftest test-e2e-numberp-true ()
  "NUMBERP - true cases"
  (is-e2e "(numberp 42)" "T")
  (is-e2e "(numberp 0)" "T")
  (is-e2e "(numberp -123)" "T"))

(deftest test-e2e-numberp-false ()
  "NUMBERP - false cases"
  (is-e2e "(numberp 'foo)" "NIL")
  (is-e2e "(numberp nil)" "NIL")
  (is-e2e "(numberp (cons 1 2))" "NIL"))

;;; ============================================================
;;; FIXNUMP Tests
;;; ============================================================

(deftest test-e2e-fixnump-true ()
  "FIXNUMP - true cases"
  (is-e2e "(fixnump 42)" "T")
  (is-e2e "(fixnump 0)" "T")
  (is-e2e "(fixnump -1)" "T"))

(deftest test-e2e-fixnump-false ()
  "FIXNUMP - false cases"
  (is-e2e "(fixnump 'foo)" "NIL")
  (is-e2e "(fixnump nil)" "NIL"))

;;; ============================================================
;;; FUNCTIONP Tests
;;; ============================================================

(deftest test-e2e-functionp-true ()
  "FUNCTIONP - true cases"
  (is-e2e "(functionp (lambda (x) x))" "T")
  (is-e2e "(functionp (lambda () 42))" "T"))

(deftest test-e2e-functionp-false ()
  "FUNCTIONP - false cases"
  (is-e2e "(functionp 42)" "NIL")
  (is-e2e "(functionp 'foo)" "NIL")
  (is-e2e "(functionp nil)" "NIL"))

;;; ============================================================
;;; STRINGP Tests
;;; ============================================================

(deftest test-e2e-stringp-true ()
  "STRINGP - true cases"
  (is-e2e "(stringp \"hello\")" "T")
  (is-e2e "(stringp \"\")" "T"))

(deftest test-e2e-stringp-false ()
  "STRINGP - false cases"
  (is-e2e "(stringp 'hello)" "NIL")
  (is-e2e "(stringp 42)" "NIL")
  (is-e2e "(stringp nil)" "NIL"))

;;; ============================================================
;;; VECTORP Tests
;;; ============================================================

(deftest test-e2e-vectorp-false-on-non-vectors ()
  "VECTORP - false on non-vectors"
  (is-e2e "(vectorp '(1 2 3))" "NIL")
  (is-e2e "(vectorp 42)" "NIL")
  (is-e2e "(vectorp 'foo)" "NIL"))

;;; ============================================================
;;; Combined Predicate Tests
;;; ============================================================

(deftest test-e2e-type-of-nil ()
  "NIL type checks"
  (is-e2e "(symbolp nil)" "T")
  (is-e2e "(null nil)" "T")
  (is-e2e "(listp nil)" "T")
  (is-e2e "(atom nil)" "T"))

(deftest test-e2e-type-of-t ()
  "T type checks"
  (is-e2e "(symbolp t)" "T")
  (is-e2e "(null t)" "NIL")
  (is-e2e "(atom t)" "T"))

(deftest test-e2e-type-exclusive ()
  "Type predicates are mutually exclusive where appropriate"
  ;; Number is not a symbol
  (is-e2e "(symbolp 42)" "NIL")
  ;; Symbol is not a number
  (is-e2e "(numberp 'foo)" "NIL")
  ;; Cons is not an atom
  (is-e2e "(atom (cons 1 2))" "NIL"))
