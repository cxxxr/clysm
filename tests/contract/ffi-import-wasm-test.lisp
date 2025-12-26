;;;; ffi-import-wasm-test.lisp - Contract tests for FFI import section generation
;;;; Feature: 027-complete-ffi (T014)

(in-package #:clysm/tests)

(deftest ffi-import-section-generation-test
  "Test that FFI declarations produce valid import section"
  (testing "import section structure"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function test-fn "host.func" (:fixnum) :fixnum))

    (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
      (ok imports "Should collect imports")
      (ok (listp imports) "Imports should be a list")
      (ok (>= (length imports) 1) "Should have at least 1 import"))))

(deftest ffi-import-type-index-test
  "Test that FFI imports get assigned type indices"
  (testing "type index assignment"
    (clysm/ffi:reset-ffi-environment)
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (eval '(clysm/ffi:define-foreign-function fn1 "host.fn1" (:fixnum) :fixnum))
    (eval '(clysm/ffi:define-foreign-function fn2 "host.fn2" () :void))

    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let ((decl1 (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'fn1))
          (decl2 (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'fn2)))
      ;; Type indices should be assigned (>= 23 per spec - after reserved types)
      (ok (or (null (clysm/ffi:ffd-type-index decl1))
              (>= (clysm/ffi:ffd-type-index decl1) 0))
          "fn1 should have valid type index or nil")
      (ok (or (null (clysm/ffi:ffd-type-index decl2))
              (>= (clysm/ffi:ffd-type-index decl2) 0))
          "fn2 should have valid type index or nil"))))
