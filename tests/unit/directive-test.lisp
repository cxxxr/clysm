;;;; directive-test.lisp - Unit tests for compile-time directive processing
;;;; Phase 13D-3: Compile-time directive tests
;;;;
;;;; HyperSpec References:
;;;; - in-package: resources/HyperSpec/Body/m_in_pkg.htm
;;;; - defpackage: resources/HyperSpec/Body/m_defpkg.htm
;;;; - declaim: resources/HyperSpec/Body/m_declai.htm
;;;; - proclaim: resources/HyperSpec/Body/f_procla.htm

(defpackage #:clysm/tests/unit/directive
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler
                #:directive-form-p
                #:compile-toplevel-form
                #:eval-directive))

(in-package #:clysm/tests/unit/directive)

;;; ============================================================
;;; Test Suite: directive-form-p predicate
;;; ============================================================

(deftest directive-form-p-in-package-tests
  "Test directive-form-p recognizes in-package forms"
  ;; T009: Unit test for directive-form-p with in-package forms
  (testing "in-package with keyword argument"
    (ok (directive-form-p '(in-package :clysm))))
  (testing "in-package with string argument"
    (ok (directive-form-p '(in-package "CLYSM"))))
  (testing "in-package with quoted symbol"
    (ok (directive-form-p '(in-package #:clysm))))
  (testing "cl:in-package qualified form"
    (ok (directive-form-p '(cl:in-package :clysm)))))

(deftest directive-form-p-defpackage-tests
  "Test directive-form-p recognizes defpackage forms"
  ;; T016: Unit test for directive-form-p with defpackage forms
  (testing "defpackage basic form"
    (ok (directive-form-p '(defpackage :my-package))))
  (testing "defpackage with options"
    (ok (directive-form-p '(defpackage :my-package (:use :cl) (:export #:foo)))))
  (testing "cl:defpackage qualified form"
    (ok (directive-form-p '(cl:defpackage :my-package)))))

(deftest directive-form-p-declaim-proclaim-tests
  "Test directive-form-p recognizes declaim and proclaim forms"
  ;; T023: Unit test for directive-form-p with declaim/proclaim forms
  (testing "declaim optimize form"
    (ok (directive-form-p '(declaim (optimize (speed 3))))))
  (testing "declaim special form"
    (ok (directive-form-p '(declaim (special *my-var*)))))
  (testing "proclaim type form"
    (ok (directive-form-p '(proclaim '(type fixnum x)))))
  (testing "cl:declaim qualified form"
    (ok (directive-form-p '(cl:declaim (optimize (safety 1))))))
  (testing "cl:proclaim qualified form"
    (ok (directive-form-p '(cl:proclaim '(special *x*))))))

(deftest directive-form-p-negative-tests
  "Test directive-form-p returns nil for non-directive forms"
  (testing "defun is not a directive"
    (ng (directive-form-p '(defun foo () 42))))
  (testing "defvar is not a directive"
    (ng (directive-form-p '(defvar *x* 10))))
  (testing "lambda is not a directive"
    (ng (directive-form-p '(lambda (x) x))))
  (testing "setq is not a directive"
    (ng (directive-form-p '(setq x 10))))
  (testing "atom is not a directive"
    (ng (directive-form-p 42)))
  (testing "nil is not a directive"
    (ng (directive-form-p nil)))
  (testing "string is not a directive"
    (ng (directive-form-p "hello"))))

;;; ============================================================
;;; Test Suite: compile-toplevel-form function
;;; ============================================================

(deftest compile-toplevel-form-in-package-tests
  "Test compile-toplevel-form processes in-package directives"
  ;; T010: Unit test for package context change via compile-toplevel-form
  (testing "in-package returns nil (no AST)"
    (let ((result (compile-toplevel-form '(in-package :cl-user))))
      (ok (null result) "in-package should return nil")))
  (testing "in-package changes *package*"
    (let ((*package* (find-package :cl-user)))
      (compile-toplevel-form '(in-package :cl))
      (ok (eq *package* (find-package :cl)) "*package* should be CL"))))

(deftest compile-toplevel-form-defpackage-tests
  "Test compile-toplevel-form processes defpackage directives"
  ;; T017: Unit test for package creation via compile-toplevel-form
  (testing "defpackage returns nil (no AST)"
    ;; Clean up if test package exists
    (when (find-package :clysm-test-pkg-001)
      (delete-package :clysm-test-pkg-001))
    (let ((result (compile-toplevel-form '(defpackage :clysm-test-pkg-001))))
      (ok (null result) "defpackage should return nil")))
  (testing "defpackage creates package"
    (ok (find-package :clysm-test-pkg-001) "Package should exist after defpackage")
    ;; Cleanup
    (delete-package :clysm-test-pkg-001)))

(deftest compile-toplevel-form-declaim-tests
  "Test compile-toplevel-form processes declaim directives"
  ;; T024: Unit test for declaim optimize in compile-toplevel-form
  (testing "declaim optimize returns nil (no AST)"
    (let ((result (compile-toplevel-form '(declaim (optimize (speed 1))))))
      (ok (null result) "declaim should return nil"))))

(deftest compile-toplevel-form-proclaim-tests
  "Test compile-toplevel-form processes proclaim directives"
  ;; T025: Unit test for declaim special in compile-toplevel-form (proclaim variant)
  (testing "proclaim returns nil (no AST)"
    (let ((result (compile-toplevel-form '(proclaim '(optimize (safety 1))))))
      (ok (null result) "proclaim should return nil"))))

(deftest compile-toplevel-form-passthrough-tests
  "Test compile-toplevel-form passes through non-directive forms"
  (testing "defun form is passed through"
    (let* ((form '(defun foo () 42))
           (result (compile-toplevel-form form)))
      (ok (equal result form) "Non-directive forms should be returned unchanged")))
  (testing "lambda form is passed through"
    (let* ((form '(lambda (x) x))
           (result (compile-toplevel-form form)))
      (ok (equal result form) "Lambda form should be returned unchanged"))))

;;; ============================================================
;;; Test Suite: Error handling
;;; ============================================================

(deftest in-package-error-tests
  "Test error handling for in-package with non-existent package"
  ;; T012: Unit test for error on non-existent package
  (testing "in-package with non-existent package signals error"
    (ok (signals (compile-toplevel-form '(in-package :this-package-does-not-exist-12345))
                 'error))))

(deftest defpackage-integration-tests
  "Test defpackage followed by in-package"
  ;; T019: Integration test for defpackage followed by in-package
  (testing "defpackage then in-package works"
    ;; Clean up if test package exists
    (when (find-package :clysm-test-pkg-integration)
      (delete-package :clysm-test-pkg-integration))
    (let ((*package* (find-package :cl-user)))
      ;; Create package
      (compile-toplevel-form '(defpackage :clysm-test-pkg-integration (:use :cl)))
      ;; Switch to it
      (compile-toplevel-form '(in-package :clysm-test-pkg-integration))
      (ok (eq *package* (find-package :clysm-test-pkg-integration))
          "*package* should be the newly created package")
      ;; Cleanup
      (in-package :cl-user)
      (delete-package :clysm-test-pkg-integration))))
