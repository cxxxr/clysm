;;;; directive-output-test.lisp - Contract tests for directive Wasm output
;;;; Phase 13D-3: Verify directive forms produce no AST/Wasm
;;;;
;;;; HyperSpec References:
;;;; - in-package: resources/HyperSpec/Body/m_in_pkg.htm
;;;; - defpackage: resources/HyperSpec/Body/m_defpkg.htm
;;;; - declaim: resources/HyperSpec/Body/m_declai.htm
;;;; - proclaim: resources/HyperSpec/Body/f_procla.htm

(defpackage #:clysm/tests/contract/directive-output
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler
                #:compile-toplevel-form))

(in-package #:clysm/tests/contract/directive-output)

;;; ============================================================
;;; Contract Tests: No AST Generation for Directives
;;; ============================================================

(deftest in-package-no-ast-test
  "Contract: in-package MUST return nil (no AST generation)"
  ;; T011: Contract test verifying nil return (no AST) for in-package
  (testing "in-package returns nil for keyword package"
    (let ((result (compile-toplevel-form '(in-package :cl-user))))
      (ok (null result) "FR-005: in-package MUST NOT generate AST")))
  (testing "in-package returns nil for string package"
    (let ((result (compile-toplevel-form '(in-package "CL-USER"))))
      (ok (null result) "FR-005: in-package MUST NOT generate AST")))
  (testing "cl:in-package returns nil"
    (let ((result (compile-toplevel-form '(cl:in-package :cl-user))))
      (ok (null result) "FR-005: qualified in-package MUST NOT generate AST"))))

(deftest defpackage-no-ast-test
  "Contract: defpackage MUST return nil (no AST generation)"
  ;; T018: Contract test verifying nil return (no AST) for defpackage
  (testing "defpackage returns nil for simple form"
    ;; Clean up if exists
    (when (find-package :clysm-contract-test-pkg)
      (delete-package :clysm-contract-test-pkg))
    (let ((result (compile-toplevel-form '(defpackage :clysm-contract-test-pkg))))
      (ok (null result) "FR-005: defpackage MUST NOT generate AST")
      ;; Cleanup
      (when (find-package :clysm-contract-test-pkg)
        (delete-package :clysm-contract-test-pkg))))
  (testing "defpackage returns nil for complex form"
    ;; Clean up if exists
    (when (find-package :clysm-contract-test-pkg-2)
      (delete-package :clysm-contract-test-pkg-2))
    (let ((result (compile-toplevel-form
                   '(defpackage :clysm-contract-test-pkg-2
                      (:use :cl)
                      (:export #:foo #:bar)
                      (:nicknames :ctp2)))))
      (ok (null result) "FR-005: defpackage with options MUST NOT generate AST")
      ;; Cleanup
      (when (find-package :clysm-contract-test-pkg-2)
        (delete-package :clysm-contract-test-pkg-2)))))

(deftest declaim-no-ast-test
  "Contract: declaim MUST return nil (no AST generation)"
  ;; T026: Contract test verifying nil return (no AST) for declaim/proclaim
  (testing "declaim optimize returns nil"
    (let ((result (compile-toplevel-form '(declaim (optimize (speed 3) (safety 0))))))
      (ok (null result) "FR-005: declaim MUST NOT generate AST")))
  (testing "declaim special returns nil"
    (let ((result (compile-toplevel-form '(declaim (special *test-special-var*)))))
      (ok (null result) "FR-005: declaim special MUST NOT generate AST")))
  (testing "cl:declaim returns nil"
    (let ((result (compile-toplevel-form '(cl:declaim (optimize (debug 1))))))
      (ok (null result) "FR-005: qualified declaim MUST NOT generate AST"))))

(deftest proclaim-no-ast-test
  "Contract: proclaim MUST return nil (no AST generation)"
  ;; T026: Contract test verifying nil return (no AST) for declaim/proclaim
  (testing "proclaim type returns nil"
    (let ((result (compile-toplevel-form '(proclaim '(type fixnum x)))))
      (ok (null result) "FR-005: proclaim MUST NOT generate AST")))
  (testing "proclaim optimize returns nil"
    (let ((result (compile-toplevel-form '(proclaim '(optimize (safety 1))))))
      (ok (null result) "FR-005: proclaim optimize MUST NOT generate AST")))
  (testing "cl:proclaim returns nil"
    (let ((result (compile-toplevel-form '(cl:proclaim '(special *x*)))))
      (ok (null result) "FR-005: qualified proclaim MUST NOT generate AST"))))

;;; ============================================================
;;; Contract Tests: Compile-Time Environment Modification
;;; ============================================================

(deftest in-package-modifies-environment-test
  "Contract: in-package MUST modify compile-time *package*"
  ;; FR-007: Directive evaluation MUST modify the compile-time environment
  (testing "in-package changes *package* binding"
    (let ((*package* (find-package :cl-user)))
      (compile-toplevel-form '(in-package :cl))
      (ok (eq *package* (find-package :cl))
          "FR-007: in-package MUST modify *package*"))))

(deftest defpackage-creates-package-test
  "Contract: defpackage MUST create package at compile-time"
  ;; FR-007: Directive evaluation MUST modify the compile-time environment
  (testing "defpackage creates package"
    ;; Clean up if exists
    (when (find-package :clysm-env-test-pkg)
      (delete-package :clysm-env-test-pkg))
    (compile-toplevel-form '(defpackage :clysm-env-test-pkg (:use :cl)))
    (ok (find-package :clysm-env-test-pkg)
        "FR-007: defpackage MUST create package at compile-time")
    ;; Cleanup
    (delete-package :clysm-env-test-pkg)))

;;; ============================================================
;;; Contract Tests: Error Signaling
;;; ============================================================

(deftest directive-error-signaling-test
  "Contract: Directive errors MUST be signaled as compile-time errors"
  ;; FR-008: Errors MUST be signaled as compile-time errors with clear messages
  (testing "non-existent package signals error"
    (ok (signals (compile-toplevel-form '(in-package :absolutely-nonexistent-package-xyz))
                 'error)
        "FR-008: undefined package MUST signal compile-time error")))
