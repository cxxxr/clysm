;;;; test-packagep.lisp - Unit tests for packagep* FFI stub
;;;; Task: T027 [US3]
;;;;
;;;; TDD: Tests written FIRST before implementation.
;;;; These tests must FAIL initially (T028).

(in-package #:clysm/tests)

(deftest test-packagep*-true ()
  "Test that packagep* returns true for package objects."
  (let ((pkg (find-package "CL")))
    (ok (clysm::packagep* pkg)
        "packagep* should return true for a package object")))

(deftest test-packagep*-false-symbol ()
  "Test that packagep* returns false for non-package objects."
  (ok (not (clysm::packagep* 'foo))
      "packagep* should return false for symbol")
  (ok (not (clysm::packagep* "string"))
      "packagep* should return false for string")
  (ok (not (clysm::packagep* 42))
      "packagep* should return false for number")
  (ok (not (clysm::packagep* nil))
      "packagep* should return false for nil"))

(deftest test-packagep*-consistency ()
  "Test that packagep* matches packagep for all test cases."
  (let ((test-values (list (find-package "CL")
                           (find-package "KEYWORD")
                           'foo
                           "string"
                           42
                           nil
                           '(1 2 3))))
    (dolist (val test-values)
      (ok (eq (clysm::packagep* val) (packagep val))
          (format nil "packagep* should match packagep for ~S" val)))))

(deftest test-packagep*-compilation ()
  "Test that a defun using packagep* compiles to Wasm."
  (let ((form '(defun test-pkgp (x) (clysm::packagep* x))))
    (let ((result (compile-form-to-wasm form)))
      (ok result
          "Form using packagep* should compile")
      (ok (search "host" (wasm-output-imports result))
          "Compiled output should include host FFI import"))))

(deftest test-packagep*-in-cond ()
  "Test packagep* usage in conditional context."
  (let ((form '(defun pkg-or-nil (x)
                 (if (clysm::packagep* x)
                     x
                     nil))))
    (let ((result (compile-form-to-wasm form)))
      (ok result
          "Conditional form using packagep* should compile"))))
