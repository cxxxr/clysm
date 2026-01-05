;;;; tests/e2e/unwind-test.lisp - E2E Tests for Unwind-protect
;;;;
;;;; Tests that UNWIND-PROTECT works correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-unwind-suite "E2E tests for unwind-protect")

;;; ============================================================
;;; Basic UNWIND-PROTECT Tests
;;; ============================================================

(deftest test-e2e-unwind-protect-normal ()
  "Unwind-protect executes cleanup after normal return"
  (is-e2e "(let ((x 0))
            (unwind-protect
                (setq x 1)
              (setq x (+ x 10)))
            x)"
          "11"))

(deftest test-e2e-unwind-protect-returns-protected ()
  "Unwind-protect returns protected form value"
  (is-e2e "(unwind-protect 42 1 2 3)" "42"))

(deftest test-e2e-unwind-protect-cleanup-runs ()
  "Cleanup forms always run"
  (is-e2e "(let ((log nil))
            (unwind-protect
                (setq log (cons 'body log))
              (setq log (cons 'cleanup log)))
            log)"
          "(CLEANUP BODY)"))

;;; ============================================================
;;; UNWIND-PROTECT with Non-local Exit
;;; ============================================================

(deftest test-e2e-unwind-protect-with-block ()
  "Cleanup runs on return-from"
  (is-e2e "(let ((x 0))
            (block outer
              (unwind-protect
                  (return-from outer 'early)
                (setq x 99)))
            x)"
          "99"))

(deftest test-e2e-unwind-protect-with-catch ()
  "Cleanup runs on throw"
  (is-e2e "(let ((x 0))
            (catch 'bail
              (unwind-protect
                  (throw 'bail 'escaped)
                (setq x 42)))
            x)"
          "42"))

;;; ============================================================
;;; Nested UNWIND-PROTECT
;;; ============================================================

(deftest test-e2e-unwind-protect-nested ()
  "Nested unwind-protect"
  (is-e2e "(let ((x 0))
            (unwind-protect
                (unwind-protect
                    (setq x 1)
                  (setq x (+ x 10)))
              (setq x (+ x 100)))
            x)"
          "111"))

;;; ============================================================
;;; UNWIND-PROTECT Return Values
;;; ============================================================

(deftest test-e2e-unwind-protect-cleanup-value-ignored ()
  "Cleanup form values are ignored"
  (is-e2e "(unwind-protect 'protected 'cleanup1 'cleanup2)" "PROTECTED"))

(deftest test-e2e-unwind-protect-nil-protected ()
  "Protected form can return NIL"
  (is-e2e "(unwind-protect nil 42)" "NIL"))
