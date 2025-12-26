;;;; export-function-test.lisp - Unit tests for FFI export registration
;;;; Feature: 027-complete-ffi (T024)

(in-package #:clysm/tests)

(deftest export-function-registration-test
  "Test that export-function registers declarations in *ffi-environment*"
  (testing "basic export registration"
    (clysm/ffi:reset-ffi-environment)

    ;; Define a Lisp function to export
    (defun test-my-add (a b) (+ a b))

    ;; Export it
    (eval '(clysm/ffi:export-function test-my-add
            :as "my_add"
            :signature ((:fixnum :fixnum) :fixnum)))

    ;; Verify registration
    (let ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
      (ok exports "Exports list should not be empty")
      (ok (= 1 (length exports)) "Should have 1 export")

      (let ((decl (first exports)))
        (ok (eq (clysm/ffi:ed-lisp-name decl) 'test-my-add)
            "Lisp name should match")
        (ok (string= (clysm/ffi:ed-export-name decl) "my_add")
            "Export name should be 'my_add'")
        (ok (equal (clysm/ffi:ed-param-types decl) '(:fixnum :fixnum))
            "Param types should be (:fixnum :fixnum)")
        (ok (eq (clysm/ffi:ed-return-type decl) :fixnum)
            "Return type should be :fixnum")))))

(deftest export-function-default-name-test
  "Test export-function with default export name"
  (testing "default export name from lisp-name"
    (clysm/ffi:reset-ffi-environment)

    (defun test-helper () 42)
    (eval '(clysm/ffi:export-function test-helper
            :signature (() :fixnum)))

    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (first exports)))
      (ok decl "Export should be registered")
      ;; Default name is lowercase of symbol-name
      (ok (string= (clysm/ffi:ed-export-name decl) "test-helper")
          "Default export name should be lowercase of symbol name"))))

(deftest export-function-void-return-test
  "Test export-function with void return type"
  (testing "void return type"
    (clysm/ffi:reset-ffi-environment)

    (defun test-log (msg) (declare (ignore msg)))
    (eval '(clysm/ffi:export-function test-log
            :as "log"
            :signature ((:string) :void)))

    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (first exports)))
      (ok (eq (clysm/ffi:ed-return-type decl) :void)
          "Return type should be :void"))))
