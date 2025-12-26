;;;; ffi-export-test-027.lisp - Integration tests for FFI export
;;;; Feature: 027-complete-ffi (T028)

(in-package #:clysm/tests)

(deftest ffi-export-integration-test
  "Test complete FFI export workflow from declaration to Wasm"
  (testing "end-to-end FFI export"
    (clysm/ffi:reset-ffi-environment)

    ;; Define a Lisp function
    (defun test-export-add (a b) (+ a b))

    ;; Export it
    (eval '(clysm/ffi:export-function test-export-add
            :as "add_numbers"
            :signature ((:fixnum :fixnum) :fixnum)))

    ;; Assign indices
    (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)

    ;; Collect and verify
    (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
      (ok exports "Should have exports")
      (ok (= 1 (length exports)) "Should have 1 export")

      (let ((decl (first exports)))
        (ok (string= (clysm/ffi:ed-export-name decl) "add_numbers")
            "Export name should be 'add_numbers'")))))

(deftest ffi-export-wrapper-generation-test
  "Test that export wrappers are generated correctly"
  (testing "wrapper generation"
    (clysm/ffi:reset-ffi-environment)

    (defun test-square (x) (* x x))
    (eval '(clysm/ffi:export-function test-square
            :as "square"
            :signature ((:fixnum) :fixnum)))

    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (first exports)))
      ;; Generate wrapper
      (let ((wrapper (clysm/ffi:generate-export-wrapper decl)))
        (ok wrapper "Wrapper should be generated")
        (ok (eq (car wrapper) :func) "Wrapper should be a function")))))

(deftest ffi-export-section-emit-test
  "Test that export section can be emitted"
  (testing "section emission"
    (clysm/ffi:reset-ffi-environment)

    (defun test-noop () nil)
    (eval '(clysm/ffi:export-function test-noop
            :as "noop"
            :signature (() :void)))

    (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)

    ;; Emit to buffer
    (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
      (let ((count (clysm/ffi:emit-ffi-exports clysm/ffi:*ffi-environment* buffer)))
        (ok (= count 1) "Should emit 1 export")
        (ok (> (length buffer) 0) "Buffer should have content")))))
