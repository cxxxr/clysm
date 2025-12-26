;;;; ffi-condition-test.lisp - Unit tests for FFI condition types
;;;; Feature: 027-complete-ffi (T034)

(in-package #:clysm/tests)

(deftest ffi-host-error-condition-test
  "Test ffi-host-error condition structure"
  (testing "condition slots"
    ;; ffi-host-error should have :function-name and :message slots
    (let ((err (make-condition 'clysm/ffi:ffi-host-error
                               :function-name "host.test"
                               :message "Test error message")))
      (ok (string= (clysm/ffi:ffi-host-error-function-name err) "host.test")
          "function-name slot should work")
      (ok (string= (clysm/ffi:ffi-host-error-message err) "Test error message")
          "message slot should work")))

  (testing "condition reporting"
    (let ((err (make-condition 'clysm/ffi:ffi-host-error
                               :function-name "host.fn"
                               :message "oops")))
      (let ((msg (format nil "~A" err)))
        (ok (search "host.fn" msg) "Report should include function name")
        (ok (search "oops" msg) "Report should include message")))))

(deftest ffi-type-error-condition-test
  "Test ffi-type-error condition structure"
  (testing "condition slots"
    (let ((err (make-condition 'clysm/ffi:ffi-type-error
                               :expected-type :fixnum
                               :actual-value "not a number")))
      (ok (eq (clysm/ffi:ffi-type-error-expected-type err) :fixnum)
          "expected-type slot should work")
      (ok (string= (clysm/ffi:ffi-type-error-actual-value err) "not a number")
          "actual-value slot should work")))

  (testing "condition reporting"
    (let ((err (make-condition 'clysm/ffi:ffi-type-error
                               :expected-type :fixnum
                               :actual-value 3.14)))
      (let ((msg (format nil "~A" err)))
        (ok (search "FIXNUM" (string-upcase msg))
            "Report should include expected type")))))

(deftest ffi-error-signaling-test
  "Test that FFI errors can be signaled and caught"
  (testing "host error signaling"
    (handler-case
        (error 'clysm/ffi:ffi-host-error
               :function-name "test"
               :message "intentional")
      (clysm/ffi:ffi-host-error (e)
        (ok t "Should catch ffi-host-error")
        (ok (string= (clysm/ffi:ffi-host-error-function-name e) "test")
            "Should have correct function name"))))

  (testing "type error signaling"
    (handler-case
        (error 'clysm/ffi:ffi-type-error
               :expected-type :string
               :actual-value 42)
      (clysm/ffi:ffi-type-error (e)
        (ok t "Should catch ffi-type-error")
        (ok (eq (clysm/ffi:ffi-type-error-expected-type e) :string)
            "Should have correct expected type")))))
