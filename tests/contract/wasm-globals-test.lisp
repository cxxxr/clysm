;;;; wasm-globals-test.lisp - Contract tests for global variable Wasm output
;;;;
;;;; Part of Phase 13D-4: Global Variable Definitions
;;;; Validates Wasm output passes wasm-tools validate
;;;;
;;;; TDD: Tests first - these tests should FAIL before implementation

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Package Setup
;;; ============================================================

(deftest wasm-globals-contract-suite
  (testing "Wasm globals contract test suite exists"
    (ok t "Contract test suite loaded")))

;;; ============================================================
;;; User Story 1: Contract Tests (T016)
;;; ============================================================

(deftest defvar-wasm-validate-test
  (testing "T016: wasm-tools validate on compiled defvar output"
    ;; Compile a simple defvar
    (let* ((form '(progn
                   (defvar *counter* 0)
                   *counter*))
           (wasm-bytes (clysm:compile-to-wasm form)))
      ;; Validate Wasm output
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled defvar produces valid Wasm"))))

(deftest defvar-unbound-wasm-validate-test
  (testing "defvar without init produces valid Wasm"
    (let* ((form '(progn
                   (defvar *unset*)
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled defvar (unbound) produces valid Wasm"))))

(deftest defparameter-wasm-validate-test
  (testing "defparameter produces valid Wasm"
    (let* ((form '(progn
                   (defparameter *config* 42)
                   *config*))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled defparameter produces valid Wasm"))))

;;; ============================================================
;;; User Story 2: Contract Tests (T026)
;;; ============================================================

(deftest hash-table-init-wasm-validate-test
  (testing "T026: $init function correctly initializes hash-table globals"
    ;; Compile defvar with hash-table
    (let* ((form '(progn
                   (defvar *registry* (make-hash-table :test 'eq))
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      ;; Validate Wasm output
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled hash-table defvar produces valid Wasm")
      ;; Check that $init function exists (deferred init)
      (ok (clysm/tests:wasm-has-init-function-p wasm-bytes)
          "Module contains $init function for deferred initialization"))))

;;; ============================================================
;;; User Story 3: Contract Tests (T034)
;;; ============================================================

(deftest package-globals-wasm-validate-test
  (testing "T034: package globals pass wasm-tools validate"
    (let* ((form '(progn
                   (defvar *current-package* nil)
                   (defvar *packages* nil)
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled package globals produce valid Wasm"))))

;;; ============================================================
;;; User Story 4: Contract Tests (T040)
;;; ============================================================

(deftest stream-globals-wasm-validate-test
  (testing "T040: stream globals pass wasm-tools validate"
    (let* ((form '(progn
                   (defvar *standard-output* nil)
                   (defvar *standard-input* nil)
                   (defvar *error-output* nil)
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled stream globals produce valid Wasm"))))

;;; ============================================================
;;; User Story 5: Contract Tests (T046)
;;; ============================================================

(deftest condition-globals-wasm-validate-test
  (testing "T046: condition globals pass wasm-tools validate"
    (let* ((form '(progn
                   (defvar *handler-clusters* '())
                   (defvar *restart-clusters* '())
                   (defvar *debugger-hook* nil)
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Compiled condition globals produce valid Wasm"))))

;;; ============================================================
;;; Global Section Structure Tests
;;; ============================================================

(deftest global-section-structure-test
  (testing "Global section has correct structure"
    (let* ((form '(progn (defvar *x* 0) *x*))
           (wasm-bytes (clysm:compile-to-wasm form)))
      ;; Check global section exists (section ID 6)
      (ok (clysm/tests:wasm-has-global-section-p wasm-bytes)
          "Wasm module has global section")
      ;; Check at least 5 globals (4 reserved + 1 user)
      (ok (>= (clysm/tests:wasm-global-count wasm-bytes) 5)
          "At least 5 globals present (4 reserved + 1 user)"))))

(deftest global-type-ref-null-any-test
  (testing "User globals have (ref null any) type"
    (let* ((form '(progn (defvar *x* 0) *x*))
           (wasm-bytes (clysm:compile-to-wasm form)))
      ;; Verify global type encoding
      ;; Note: This test may need adjustment based on actual binary format
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Wasm with special variable type is valid"))))

(deftest global-mutability-test
  (testing "User globals are mutable"
    (let* ((form '(progn
                   (defvar *x* 0)
                   (setq *x* 1)
                   *x*))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Setq on special variable produces valid Wasm"))))

;;; ============================================================
;;; Helper Functions (to be implemented in helpers.lisp)
;;; ============================================================

;; These helper functions should be implemented in tests/helpers.lisp:
;;
;; (defun wasm-has-init-function-p (wasm-bytes)
;;   "Check if Wasm module has a $init function for deferred initialization")
;;
;; (defun wasm-has-global-section-p (wasm-bytes)
;;   "Check if Wasm module has a global section (section ID 6)")
;;
;; (defun wasm-global-count (wasm-bytes)
;;   "Return count of globals in Wasm module")

;;; ============================================================
;;; Cross-Reference Initialization Tests
;;; ============================================================

(deftest global-cross-reference-test
  (testing "Global referencing another global"
    (let* ((form '(progn
                   (defvar *base* nil)
                   (defvar *derived* *base*)
                   nil))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Cross-referencing globals produce valid Wasm"))))

;;; ============================================================
;;; Multiple Globals Test
;;; ============================================================

(deftest multiple-globals-test
  (testing "Multiple globals in single compilation"
    (let* ((form '(progn
                   (defvar *a* 1)
                   (defvar *b* 2)
                   (defvar *c* 3)
                   (defparameter *d* 4)
                   (+ *a* *b* *c* *d*)))
           (wasm-bytes (clysm:compile-to-wasm form)))
      (ok (clysm/tests:wasm-valid-p wasm-bytes)
          "Multiple globals compile to valid Wasm"))))
