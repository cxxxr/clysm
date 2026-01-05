;;;; tests/e2e/let-test.lisp - E2E Tests for Variable Binding
;;;;
;;;; Tests that LET, LET*, and SETQ work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-let-suite "E2E tests for variable binding")

;;; ============================================================
;;; Basic LET Tests
;;; ============================================================

(deftest test-e2e-let-single ()
  "LET with single binding"
  (is-e2e "(let ((x 10)) x)" "10")
  (is-e2e "(let ((y 42)) y)" "42"))

(deftest test-e2e-let-multiple ()
  "LET with multiple bindings"
  (is-e2e "(let ((x 10) (y 20)) (+ x y))" "30")
  (is-e2e "(let ((a 1) (b 2) (c 3)) (+ a (+ b c)))" "6"))

(deftest test-e2e-let-nil-init ()
  "LET with NIL initialization"
  (is-e2e "(let ((x nil)) x)" "NIL"))

(deftest test-e2e-let-computed-init ()
  "LET with computed initial values"
  (is-e2e "(let ((x (+ 1 2))) x)" "3")
  (is-e2e "(let ((x (cons 1 2))) (car x))" "1"))

;;; ============================================================
;;; LET Shadowing Tests
;;; ============================================================

(deftest test-e2e-let-shadow ()
  "LET shadows outer bindings"
  (is-e2e "(let ((x 1)) (let ((x 2)) x))" "2"))

(deftest test-e2e-let-shadow-outer-visible ()
  "Outer binding visible after inner scope"
  (is-e2e "(let ((x 1)) (progn (let ((x 2)) x) x))" "1"))

(deftest test-e2e-let-parallel-binding ()
  "LET bindings are parallel (not sequential)"
  ;; In parallel binding, y uses the OUTER x, not the one being bound
  (is-e2e "(let ((x 1)) (let ((x 10) (y x)) y))" "1"))

;;; ============================================================
;;; LET* Tests (Sequential Binding)
;;; ============================================================

(deftest test-e2e-let*-sequential ()
  "LET* bindings are sequential"
  (is-e2e "(let* ((x 1) (y (+ x 1))) y)" "2"))

(deftest test-e2e-let*-chain ()
  "LET* chain of dependent bindings"
  (is-e2e "(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)" "3")
  (is-e2e "(let* ((x 2) (y (* x 3)) (z (* y 4))) z)" "24"))

(deftest test-e2e-let*-shadow ()
  "LET* can shadow within same form"
  (is-e2e "(let* ((x 1) (x (+ x 1))) x)" "2"))

;;; ============================================================
;;; SETQ Tests
;;; ============================================================

(deftest test-e2e-setq-simple ()
  "Simple SETQ"
  (is-e2e "(let ((x 1)) (setq x 10) x)" "10"))

(deftest test-e2e-setq-returns-value ()
  "SETQ returns the assigned value"
  (is-e2e "(let ((x 1)) (setq x 42))" "42"))

(deftest test-e2e-setq-multiple ()
  "SETQ with multiple pairs"
  (is-e2e "(let ((x 1) (y 2)) (setq x 10 y 20) (+ x y))" "30"))

(deftest test-e2e-setq-computed ()
  "SETQ with computed value"
  (is-e2e "(let ((x 1)) (setq x (+ x 1)) x)" "2")
  (is-e2e "(let ((x 5)) (setq x (* x x)) x)" "25"))

;;; ============================================================
;;; Complex Binding Patterns
;;; ============================================================

(deftest test-e2e-let-body-multiple ()
  "LET with multiple body forms"
  (is-e2e "(let ((x 1)) (setq x 2) (setq x 3) x)" "3"))

(deftest test-e2e-let-nested ()
  "Nested LET forms"
  (is-e2e "(let ((x 1)) (let ((y 2)) (+ x y)))" "3"))

(deftest test-e2e-let-deeply-nested ()
  "Deeply nested LET"
  (is-e2e "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ a (+ b c)))))" "6"))

(deftest test-e2e-let-with-if ()
  "LET with IF in body"
  (is-e2e "(let ((x 5)) (if (> x 3) 'big 'small))" "BIG")
  (is-e2e "(let ((x 2)) (if (> x 3) 'big 'small))" "SMALL"))

(deftest test-e2e-let-with-progn ()
  "LET with PROGN in body"
  (is-e2e "(let ((x 0)) (progn (setq x 1) (setq x 2) x))" "2"))

;;; ============================================================
;;; Scope Tests
;;; ============================================================

(deftest test-e2e-let-scope-isolation ()
  "Variables don't leak out of LET"
  ;; After inner let, x should be the outer value
  (is-e2e "(let ((x 'outer))
            (let ((result (let ((x 'inner)) x)))
              (cons result x)))"
          "(INNER . OUTER)"))
