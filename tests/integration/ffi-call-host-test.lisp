;;;; ffi-call-host-test.lisp - Integration tests for ffi:call-host dynamic dispatch
;;;; Feature: 027-complete-ffi (T045)

(in-package #:clysm/tests)

(deftest ffi-call-host-integration-test
  "Test complete ffi:call-host workflow"
  (testing "dynamic host function invocation"
    (clysm/ffi:reset-ffi-environment)

    ;; Parse and compile a dynamic call
    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "host.random")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      ;; Should compile without error
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Dynamic call should compile"))))

  (testing "dynamic call with string argument"
    (clysm/ffi:reset-ffi-environment)

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "console.log" "Hello from Lisp")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "String argument call should compile"))))

  (testing "dynamic call with numeric arguments"
    (clysm/ffi:reset-ffi-environment)

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "host.add" 10 20)))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Numeric arguments call should compile")))))

(deftest ffi-call-host-error-handling-test
  "Test error handling for dynamic calls"
  (testing "unknown function handling"
    ;; Dynamic calls to unknown functions should be possible
    ;; (error happens at runtime, not compile time)
    (clysm/ffi:reset-ffi-environment)

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "nonexistent.function")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      ;; Should compile - error happens at runtime
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Unknown function call should compile (runtime check)")))))

(deftest ffi-call-host-vs-static-test
  "Test difference between dynamic and static FFI calls"
  (testing "static vs dynamic dispatch"
    (clysm/ffi:reset-ffi-environment)

    ;; Define a static FFI function
    (eval '(clysm/ffi:define-foreign-function static-log "host.log" (:string) :void))

    ;; Static call - uses declared function
    (let ((static-ast (clysm/compiler/ast:parse-expr '(static-log "test"))))
      (ok (typep static-ast 'clysm/compiler/ast:ast-ffi-call)
          "Static call should use ast-ffi-call"))

    ;; Dynamic call - uses call-host
    (let ((dynamic-ast (clysm/compiler/ast:parse-expr
                        '(clysm/ffi:call-host "host.log" "test"))))
      (ok (typep dynamic-ast 'clysm/compiler/ast:ast-call-host)
          "Dynamic call should use ast-call-host"))))

(deftest ffi-call-host-nested-test
  "Test nested dynamic calls"
  (testing "call-host in expression context"
    (clysm/ffi:reset-ffi-environment)

    ;; Dynamic call as argument to another expression
    ;; Note: This tests that call-host returns a value
    (let ((ast (clysm/compiler/ast:parse-expr
                '(if (clysm/ffi:call-host "host.check") 1 2))))
      (ok (typep ast 'clysm/compiler/ast:ast-if)
          "Should parse if expression")
      ;; The condition should be a call-host
      (ok (typep (clysm/compiler/ast:ast-if-condition ast)
                 'clysm/compiler/ast:ast-call-host)
          "Condition should be call-host"))))
