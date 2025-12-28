;;;; run-tests.lisp - Stage 0 test runner configuration
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T003: Test runner configuration for stage0 tests
;;;;
;;;; Usage:
;;;;   sbcl --load tests/unit/stage0/run-tests.lisp
;;;;
;;;; Or from REPL after loading clysm:
;;;;   (clysm/tests/stage0:run-stage0-tests)

(require :asdf)

(defpackage #:clysm/tests/stage0
  (:use #:cl)
  (:export #:run-stage0-tests
           #:run-test-file))

(in-package #:clysm/tests/stage0)

;;; Load the main system first (provides clysm/stage0 package)
(unless (find-package :clysm/stage0)
  (asdf:load-system :clysm))

;;; Load test framework
(unless (find-package :rove)
  (ql:quickload :rove :silent t))

;;; Define test files for stage0
(defparameter *stage0-test-files*
  '("env-test"
    "eval-test"
    "primitives-test"
    "types-test"
    "globals-test"
    "reader-test"
    "ast-test"
    "ir-test"
    "ffi-test"
    "fs-read-test")
  "List of stage0 test files (without .lisp extension)")

(defparameter *stage0-test-dir*
  (merge-pathnames "tests/unit/stage0/"
                   (asdf:system-source-directory :clysm))
  "Directory containing stage0 test files")

(defun run-test-file (test-name)
  "Load and run a single test file"
  (let ((test-path (merge-pathnames (format nil "~A.lisp" test-name)
                                    *stage0-test-dir*)))
    (when (probe-file test-path)
      (format t "~&;; Loading ~A...~%" test-name)
      (load test-path)
      t)))

(defun run-stage0-tests (&key (verbose t))
  "Run all stage0 unit tests"
  (format t "~&;; ========================================~%")
  (format t ";; Stage 0 Unit Tests~%")
  (format t ";; ========================================~%")

  ;; Load all test files
  (dolist (test-file *stage0-test-files*)
    (run-test-file test-file))

  ;; Run tests via Rove
  (format t "~&;; Running tests...~%")
  (let ((packages (list (find-package :clysm/tests/unit/stage0/env-test)
                        (find-package :clysm/tests/unit/stage0/eval-test)
                        (find-package :clysm/tests/unit/stage0/primitives-test))))
    ;; Filter out NIL packages
    (setf packages (remove nil packages))

    (if packages
        (progn
          (dolist (pkg packages)
            (format t "~&;; Testing package: ~A~%" (package-name pkg))
            (rove:run pkg))
          (format t "~&;; Stage 0 tests complete.~%"))
        (format t "~&;; Warning: No test packages found. Run (asdf:test-system :clysm) instead.~%"))))

;;; Auto-run when loaded directly
(when (and (boundp '*load-pathname*)
           *load-pathname*
           (search "run-tests" (namestring *load-pathname*)))
  (run-stage0-tests))
