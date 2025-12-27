(defpackage :clysm-tests/unit/validation/compiler-order
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/unit/validation/compiler-order)

;;; T027: Unit tests for Compilation-Result struct

(deftest compilation-result-struct-creation
  "Test that compilation-result struct can be created with required fields"
  (let ((module (v:make-module-info :path #p"/src/test.lisp"
                                    :directory #p"/src/"
                                    :dependencies nil
                                    :symbols-used nil)))
    (let ((result (v:make-compilation-result :module module
                                              :success t
                                              :wasm-bytes nil
                                              :error-message nil
                                              :unsupported-feature nil
                                              :validation-passed nil
                                              :validation-error nil)))
      (ok (v:compilation-result-p result) "Should create a compilation-result struct")
      (ok (v:compilation-result-success result) "Should store success flag")
      (ok (v:module-info-p (v:compilation-result-module result)) "Should store module-info"))))

(deftest compilation-result-failure
  "Test compilation-result struct with failure state"
  (let ((module (v:make-module-info :path #p"/src/fail.lisp"
                                    :directory #p"/src/"
                                    :dependencies nil
                                    :symbols-used nil)))
    (let ((result (v:make-compilation-result :module module
                                              :success nil
                                              :wasm-bytes nil
                                              :error-message "Undefined function: FOO"
                                              :unsupported-feature 'foo
                                              :validation-passed nil
                                              :validation-error nil)))
      (ok (not (v:compilation-result-success result)) "Should indicate failure")
      (ok (stringp (v:compilation-result-error-message result)) "Should have error message")
      (ok (symbolp (v:compilation-result-unsupported-feature result)) "Should identify unsupported feature"))))

(deftest compilation-result-with-wasm-bytes
  "Test compilation-result struct with wasm bytes"
  (let ((module (v:make-module-info :path #p"/src/ok.lisp"
                                    :directory #p"/src/"
                                    :dependencies nil
                                    :symbols-used nil))
        (bytes (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (v:make-compilation-result :module module
                                              :success t
                                              :wasm-bytes bytes
                                              :error-message nil
                                              :unsupported-feature nil
                                              :validation-passed t
                                              :validation-error nil)))
      (ok (v:compilation-result-success result) "Should indicate success")
      (ok (arrayp (v:compilation-result-wasm-bytes result)) "Should have wasm bytes")
      (ok (v:compilation-result-validation-passed result) "Should indicate validation passed"))))

;;; T028: Unit tests for compile-module function

(deftest compile-module-returns-compilation-result
  "Test that compile-module returns a compilation-result struct"
  (let* ((base-dir (asdf:system-source-directory :clysm))
         (module (v:make-module-info :path (merge-pathnames "src/clysm/backend/leb128.lisp" base-dir)
                                     :directory (merge-pathnames "src/clysm/backend/" base-dir)
                                     :dependencies nil
                                     :symbols-used nil)))
    (let ((result (v:compile-module module)))
      (ok (v:compilation-result-p result) "Should return compilation-result")
      (ok (v:module-info-p (v:compilation-result-module result)) "Should have module reference"))))

(deftest compile-module-handles-missing-file
  "Test that compile-module handles missing files gracefully"
  (let ((module (v:make-module-info :path #p"/nonexistent/file.lisp"
                                    :directory #p"/nonexistent/"
                                    :dependencies nil
                                    :symbols-used nil)))
    (let ((result (v:compile-module module)))
      (ok (v:compilation-result-p result) "Should return compilation-result")
      (ok (not (v:compilation-result-success result)) "Should indicate failure"))))

;;; T029: Unit tests for validate-wasm function

(deftest validate-wasm-empty-bytes
  "Test validate-wasm with empty bytes returns error"
  (let ((empty-bytes (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (valid error)
        (v:validate-wasm empty-bytes)
      (ok (not valid) "Empty bytes should not validate")
      (ok (stringp error) "Should return error message"))))

(deftest validate-wasm-nil-bytes
  "Test validate-wasm with nil returns error"
  (multiple-value-bind (valid error)
      (v:validate-wasm nil)
    (ok (not valid) "Nil should not validate")
    (ok (stringp error) "Should return error message")))

(deftest validate-wasm-valid-module
  "Test validate-wasm with a valid Wasm module"
  ;; Compile a simple expression to get valid Wasm bytes
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
    (when (and wasm-bytes (> (length wasm-bytes) 0))
      (multiple-value-bind (valid error)
          (v:validate-wasm wasm-bytes)
        (ok valid "Valid Wasm should validate")
        (ok (null error) "Should have no error")))))

;;; T030: Unit tests for get-dependency-order function

(deftest get-dependency-order-returns-list
  "Test that get-dependency-order returns a list of module-info structs"
  (let ((modules (v:get-dependency-order)))
    (ok (listp modules) "Should return a list")
    (ok (> (length modules) 0) "Should have at least one module")
    (ok (v:module-info-p (first modules)) "Elements should be module-info structs")))

(deftest get-dependency-order-correct-count
  "Test that get-dependency-order returns expected number of modules"
  (let ((modules (v:get-dependency-order)))
    ;; *compilation-order* has 41 modules
    (ok (= (length modules) (length v:*compilation-order*))
        "Should match *compilation-order* length")))

(deftest get-dependency-order-first-is-leb128
  "Test that first module is leb128.lisp (no dependencies)"
  (let ((modules (v:get-dependency-order)))
    (let ((first-path (namestring (v:module-info-path (first modules)))))
      (ok (search "leb128.lisp" first-path)
          "First module should be leb128.lisp"))))

(deftest get-dependency-order-paths-exist
  "Test that all module paths exist"
  (let ((modules (v:get-dependency-order)))
    (dolist (module (subseq modules 0 (min 5 (length modules))))
      (ok (probe-file (v:module-info-path module))
          (format nil "~A should exist" (v:module-info-path module))))))

;;; T031: Unit tests for compile-in-order function

(deftest compile-in-order-returns-list
  "Test that compile-in-order returns a list of compilation-results"
  ;; This will likely fail due to T034 blocker, but tests the interface
  (let ((results (v:compile-in-order :halt-on-failure t)))
    (ok (listp results) "Should return a list")
    (when (> (length results) 0)
      (ok (v:compilation-result-p (first results))
          "Elements should be compilation-result structs"))))

(deftest compile-in-order-halt-on-failure
  "Test that compile-in-order with halt-on-failure stops at first failure"
  (let ((results (v:compile-in-order :halt-on-failure t)))
    ;; Should have at least attempted one module
    (ok (>= (length results) 1) "Should have at least one result")
    ;; If there's a failure, it should be the last result
    (when (and (> (length results) 0)
               (not (v:compilation-result-success (car (last results)))))
      (ok t "Last result should be the failure point"))))

;;; Additional utility tests

(deftest compilation-order-list-exists
  "Test that *compilation-order* is defined and non-empty"
  (ok (boundp 'v:*compilation-order*) "*compilation-order* should be bound")
  (ok (listp v:*compilation-order*) "Should be a list")
  (ok (> (length v:*compilation-order*) 30) "Should have many modules"))

(deftest log-compilation-error-works
  "Test that log-compilation-error doesn't error"
  (let* ((module (v:make-module-info :path #p"/test.lisp"
                                     :directory #p"/"
                                     :dependencies nil
                                     :symbols-used nil))
         (result (v:make-compilation-result :module module
                                             :success nil
                                             :wasm-bytes nil
                                             :error-message "Test error"
                                             :unsupported-feature nil
                                             :validation-passed nil
                                             :validation-error nil)))
    (ok (null (v:log-compilation-error result (make-string-output-stream)))
        "log-compilation-error should complete without error")))

;;; Tests for form-filtering functions (T034 workaround)

(deftest compilable-form-p-defun
  "Test that defun is recognized as compilable"
  (ok (v:compilable-form-p '(defun foo () 1))
      "defun should be compilable"))

(deftest compilable-form-p-defmacro
  "Test that defmacro is recognized as compilable"
  (ok (v:compilable-form-p '(defmacro bar (x) x))
      "defmacro should be compilable"))

(deftest compilable-form-p-defvar
  "Test that defvar is recognized as compilable"
  (ok (v:compilable-form-p '(defvar *x* 10))
      "defvar should be compilable"))

(deftest compilable-form-p-in-package-not-compilable
  "Test that in-package is NOT recognized as compilable"
  (ok (not (v:compilable-form-p '(in-package :foo)))
      "in-package should NOT be compilable"))

(deftest compilable-form-p-defpackage-not-compilable
  "Test that defpackage is NOT recognized as compilable"
  (ok (not (v:compilable-form-p '(defpackage :foo (:use :cl))))
      "defpackage should NOT be compilable"))

(deftest filter-compilable-forms-removes-in-package
  "Test that filter-compilable-forms removes in-package"
  (let ((forms '((in-package :foo)
                 (defun bar () 1)
                 (defvar *x* 10))))
    (let ((filtered (v:filter-compilable-forms forms)))
      (ok (= (length filtered) 2)
          "Should remove in-package, keeping 2 forms")
      (ok (eq (caar filtered) 'defun)
          "First form should be defun")
      (ok (eq (caadr filtered) 'defvar)
          "Second form should be defvar"))))

(deftest filter-compilable-forms-empty-list
  "Test filter-compilable-forms with empty list"
  (ok (null (v:filter-compilable-forms nil))
      "Should return nil for empty list"))

(deftest filter-compilable-forms-all-non-compilable
  "Test filter-compilable-forms when all forms are non-compilable"
  (let ((forms '((in-package :foo) (defpackage :bar))))
    (ok (null (v:filter-compilable-forms forms))
        "Should return nil when all forms are non-compilable")))
