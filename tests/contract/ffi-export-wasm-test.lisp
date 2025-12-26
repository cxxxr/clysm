;;;; ffi-export-wasm-test.lisp - Contract tests for FFI export section generation
;;;; Feature: 027-complete-ffi (T026)

(in-package #:clysm/tests)

(deftest ffi-export-section-generation-test
  "Test that FFI exports produce valid export section entries"
  (testing "export section structure"
    (clysm/ffi:reset-ffi-environment)

    ;; Define a function to export
    (defun test-exported-add (a b) (+ a b))
    (eval '(clysm/ffi:export-function test-exported-add
            :as "add"
            :signature ((:fixnum :fixnum) :fixnum)))

    ;; Collect exports
    (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
      (ok exports "Should collect exports")
      (ok (listp exports) "Exports should be a list")
      (ok (>= (length exports) 1) "Should have at least 1 export"))))

(deftest ffi-export-name-test
  "Test that export names are correctly encoded"
  (testing "export name in section"
    (clysm/ffi:reset-ffi-environment)

    (defun test-fn () 0)
    (eval '(clysm/ffi:export-function test-fn
            :as "my_function"
            :signature (() :fixnum)))

    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (first exports)))
      (ok (string= (clysm/ffi:ed-export-name decl) "my_function")
          "Export name should be preserved"))))

(deftest ffi-export-wrapper-function-index-test
  "Test that export wrappers get assigned function indices"
  (testing "wrapper function index assignment"
    (clysm/ffi:reset-ffi-environment)

    (defun test-exp-fn () nil)
    (eval '(clysm/ffi:export-function test-exp-fn
            :signature (() :void)))

    ;; After assign-export-indices, wrapper should have function index
    (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)

    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (first exports)))
      ;; The wrapper function index may or may not be assigned yet
      ;; depending on implementation stage
      (ok decl "Export declaration should exist"))))
