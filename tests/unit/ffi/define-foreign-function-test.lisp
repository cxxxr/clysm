;;;; define-foreign-function-test.lisp - Unit tests for FFI declaration registration
;;;; Feature: 027-complete-ffi (T011)

(in-package #:clysm/tests)

(deftest define-foreign-function-registration-test
  "Test that define-foreign-function registers declarations in *ffi-environment*"
  (testing "basic registration"
    ;; Reset FFI environment for test isolation
    (clysm/ffi:reset-ffi-environment)

    ;; Define a foreign function
    (eval '(clysm/ffi:define-foreign-function test-log "host.log" (:string) :void))

    ;; Verify it was registered
    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment*
                 'test-log)))
      (ok decl "Foreign function should be registered")
      (ok (eq (clysm/ffi:ffd-lisp-name decl) 'test-log)
          "Lisp name should match")
      (ok (string= (clysm/ffi:ffd-module-name decl) "host")
          "Module name should be 'host'")
      (ok (string= (clysm/ffi:ffd-field-name decl) "log")
          "Field name should be 'log'")
      (ok (equal (clysm/ffi:ffd-param-types decl) '(:string))
          "Param types should be (:string)")
      (ok (eq (clysm/ffi:ffd-return-type decl) :void)
          "Return type should be :void"))))

(deftest define-foreign-function-multiple-params-test
  "Test FFI declaration with multiple parameters"
  (testing "multiple parameter types"
    (clysm/ffi:reset-ffi-environment)

    (eval '(clysm/ffi:define-foreign-function test-add "host.add" (:fixnum :fixnum) :fixnum))

    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment*
                 'test-add)))
      (ok decl "Multi-param function should be registered")
      (ok (equal (clysm/ffi:ffd-param-types decl) '(:fixnum :fixnum))
          "Param types should be (:fixnum :fixnum)")
      (ok (eq (clysm/ffi:ffd-return-type decl) :fixnum)
          "Return type should be :fixnum"))))

(deftest define-foreign-function-all-types-test
  "Test FFI declarations with all marshal types"
  (testing "all marshal types"
    (clysm/ffi:reset-ffi-environment)

    ;; Test each marshal type
    (eval '(clysm/ffi:define-foreign-function test-fixnum "host.fixnum" (:fixnum) :fixnum))
    (eval '(clysm/ffi:define-foreign-function test-float "host.float" (:float) :float))
    (eval '(clysm/ffi:define-foreign-function test-string "host.string" (:string) :string))
    (eval '(clysm/ffi:define-foreign-function test-boolean "host.boolean" (:boolean) :boolean))
    (eval '(clysm/ffi:define-foreign-function test-anyref "host.anyref" (:anyref) :anyref))

    ;; Verify all were registered
    (ok (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-fixnum)
        "Fixnum function registered")
    (ok (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-float)
        "Float function registered")
    (ok (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-string)
        "String function registered")
    (ok (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-boolean)
        "Boolean function registered")
    (ok (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-anyref)
        "Anyref function registered")))

(deftest define-foreign-function-host-name-parsing-test
  "Test host name parsing (module.field format)"
  (testing "various host name formats"
    (clysm/ffi:reset-ffi-environment)

    ;; Standard format: module.field
    (eval '(clysm/ffi:define-foreign-function f1 "env.console" () :void))
    (let ((decl (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'f1)))
      (ok (string= (clysm/ffi:ffd-module-name decl) "env") "Module is 'env'")
      (ok (string= (clysm/ffi:ffd-field-name decl) "console") "Field is 'console'"))

    ;; Nested format: module.sub.field
    (eval '(clysm/ffi:define-foreign-function f2 "host.console.log" () :void))
    (let ((decl (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'f2)))
      (ok (string= (clysm/ffi:ffd-module-name decl) "host") "Module is 'host'")
      (ok (string= (clysm/ffi:ffd-field-name decl) "console.log") "Field is 'console.log'"))))
