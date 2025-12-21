;;;; main.lisp - Main test runner and re-exports

(in-package #:clysm/tests)

;;; Re-export compile-and-run from helpers
(setf (symbol-function 'compile-and-run)
      #'clysm/tests/helpers:compile-and-run)

(defun run-all-tests ()
  "Run all Clysm tests using Rove."
  (rove:run :clysm/tests))
