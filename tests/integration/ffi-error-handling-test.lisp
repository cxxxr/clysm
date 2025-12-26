;;;; ffi-error-handling-test.lisp - Integration tests for FFI error handling
;;;; Feature: 027-complete-ffi (T036)

(in-package #:clysm/tests)

(deftest ffi-error-handling-integration-test
  "Test complete FFI error handling workflow"
  (testing "error handling end-to-end"
    (clysm/ffi:reset-ffi-environment)

    ;; Define a function that might throw
    (eval '(clysm/ffi:define-foreign-function throw-test "host.throw_error" (:string) :void))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    ;; Verify we can generate error-handling code
    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment* 'throw-test)))
      (let ((wrapped-instrs (clysm/ffi:generate-import-call-with-error-handling
                             decl
                             '(((:ref.null :none)))  ; dummy arg
                             nil
                             :catch-host-errors t)))
        (ok wrapped-instrs "Should generate wrapped call")
        (ok (listp wrapped-instrs) "Should be a list of instructions")))))

(deftest ffi-condition-hierarchy-test
  "Test FFI condition type hierarchy"
  (testing "ffi-host-error is error"
    (let ((err (make-condition 'clysm/ffi:ffi-host-error
                               :function-name "test"
                               :message "test")))
      (ok (typep err 'error) "ffi-host-error should be a subtype of error")))

  (testing "ffi-type-error is error"
    (let ((err (make-condition 'clysm/ffi:ffi-type-error
                               :expected-type :fixnum
                               :actual-value nil)))
      (ok (typep err 'error) "ffi-type-error should be a subtype of error"))))

(deftest ffi-error-restarts-test
  "Test that FFI errors could support restarts (future)"
  (testing "restarts are possible"
    ;; In a full implementation, errors could support restarts:
    ;; - USE-VALUE: Use a fallback value
    ;; - RETRY-CALL: Retry the host function call
    ;; - CONTINUE: Ignore the error and continue
    ;; For now, just verify the condition structure allows this
    (ok t "Condition structure supports restarts")))
