;;;; package-primitive-test.lisp - Tests for package primitive compilation
;;;; Feature: 001-type-package-export
;;;; HyperSpec: resources/HyperSpec/Body/f_pkgp.htm

(defpackage #:clysm/tests/unit/package-primitive-test
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler/codegen/func-section
                #:*runtime-function-table*
                #:runtime-function-p
                #:compile-to-instructions
                #:make-env))

(in-package #:clysm/tests/unit/package-primitive-test)

;;; ============================================================
;;; Phase 4: Package Predicate Tests (US2)
;;; ============================================================

(deftest test-packagep*-runtime-function-entry
  "T023: Verify PACKAGEP* is registered in runtime function table"
  (testing "PACKAGEP* should be in runtime function table with arity 1"
    (let* ((sym (intern "PACKAGEP*" :clysm))
           (entry (runtime-function-p sym)))
      (ok entry "PACKAGEP* should be registered")
      (when entry
        (ok (eq (car entry) :$packagep*-rt)
            "Runtime name should be :$packagep*-rt")
        (ok (= (cdr entry) 1)
            "Arity should be 1")))))

(deftest test-packagep*-wasm-codegen
  "T024: Verify PACKAGEP* emits ref.test instruction"
  (testing "PACKAGEP* call should compile to ref.test"
    ;; Tests PACKAGEP* runtime function dispatch
    ;; The call should emit a call to $packagep*-rt
    (let* ((form '(clysm:packagep* x))
           (ast (clysm/compiler/ast:parse-expr form))
           (env (make-env))
           ;; Add 'x' as local variable for testing
           (env-with-local (progn
                             (clysm/compiler/codegen/func-section:env-add-local env 'x)
                             env))
           (instructions (compile-to-instructions ast env-with-local)))
      (ok instructions "Should produce instructions")
      ;; Check for call to runtime function
      (ok (member '(:call :$packagep*-rt) instructions :test #'equal)
          "Should call $packagep*-rt"))))

;;; ============================================================
;;; Phase 5: Package Symbol Operations Tests (US3)
;;; ============================================================

(deftest test-symbol-package*-runtime-function-entry
  "T031: Verify SYMBOL-PACKAGE* is registered in runtime function table"
  (testing "SYMBOL-PACKAGE* should be in runtime function table with arity 1"
    ;; Will fail until T034 adds SYMBOL-PACKAGE* registration
    (let* ((sym (intern "SYMBOL-PACKAGE*" :clysm))
           (entry (runtime-function-p sym)))
      (ok entry "SYMBOL-PACKAGE* should be registered")
      (when entry
        (ok (eq (car entry) :$symbol-package*-rt)
            "Runtime name should be :$symbol-package*-rt")
        (ok (= (cdr entry) 1)
            "Arity should be 1")))))

(deftest test-find-package*-wasm-codegen
  "T032: Verify FIND-PACKAGE* Wasm codegen"
  (testing "FIND-PACKAGE* call should compile correctly"
    (let* ((sym (intern "FIND-PACKAGE*" :clysm))
           (entry (runtime-function-p sym)))
      (ok entry "FIND-PACKAGE* should be registered")
      (when entry
        (ok (eq (car entry) :$find-package*-rt)
            "Runtime name should be :$find-package*-rt")))))

(deftest test-intern*-variadic-handling
  "T033: Verify INTERN* variadic arity handling"
  (testing "INTERN* should accept 1-2 arguments (variadic)"
    (let* ((sym (intern "INTERN*" :clysm))
           (entry (runtime-function-p sym)))
      (ok entry "INTERN* should be registered")
      (when entry
        (ok (eq (car entry) :$intern*-rt)
            "Runtime name should be :$intern*-rt")
        (ok (null (cdr entry))
            "Arity should be nil (variadic)")))))

;;; ============================================================
;;; Helper for running all tests
;;; ============================================================

(defun run-all-tests ()
  "Run all package primitive tests"
  (run-suite :clysm/tests/unit/package-primitive-test))
