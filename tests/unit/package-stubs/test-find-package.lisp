;;;; test-find-package.lisp - Unit tests for find-package* FFI stub
;;;; Task: T025 [US3]
;;;;
;;;; TDD: Tests written FIRST before implementation.
;;;; These tests must FAIL initially (T028).

(in-package #:clysm/tests)

(deftest test-find-package*-basic ()
  "Test that find-package* returns a package object for known package."
  ;; Find the COMMON-LISP package
  (let ((result (clysm::find-package* "CL")))
    (ok (not (null result))
        "find-package* should return non-nil for 'CL'")
    (ok (packagep result)
        "find-package* should return a package object")))

(deftest test-find-package*-full-name ()
  "Test that find-package* works with full package names."
  (let ((result (clysm::find-package* "COMMON-LISP")))
    (ok (not (null result))
        "find-package* should find COMMON-LISP")
    (ok (eq result (find-package "CL"))
        "find-package* should return same package as find-package")))

(deftest test-find-package*-nonexistent ()
  "Test that find-package* returns nil for non-existent package."
  (let ((result (clysm::find-package* "NONEXISTENT-PACKAGE-XYZ")))
    (ok (null result)
        "find-package* should return nil for non-existent package")))

(deftest test-find-package*-keyword ()
  "Test that find-package* finds the KEYWORD package."
  (let ((result (clysm::find-package* "KEYWORD")))
    (ok (not (null result))
        "find-package* should find KEYWORD package")
    (ok (eq result (find-package :keyword))
        "find-package* should return the KEYWORD package")))

(deftest test-find-package*-compilation ()
  "Test that a defun using find-package* compiles to Wasm."
  ;; This form should compile without error
  (let ((form '(defun test-pkg () (clysm::find-package* "CL"))))
    (let ((result (compile-form-to-wasm form)))
      (ok result
          "Form using find-package* should compile")
      ;; Verify FFI import is in output
      (ok (search "host" (wasm-output-imports result))
          "Compiled output should include host FFI import"))))
