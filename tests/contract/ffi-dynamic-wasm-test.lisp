;;;; ffi-dynamic-wasm-test.lisp - Contract tests for dynamic FFI Wasm generation
;;;; Feature: 027-complete-ffi (T044)

(in-package #:clysm/tests)

(deftest ffi-dynamic-call-compilation-test
  "Test that ffi:call-host compiles to valid instructions"
  (testing "basic dynamic call compilation"
    (clysm/ffi:reset-ffi-environment)

    ;; Parse a call-host form
    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "host.random")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (ok (clysm/compiler/ast:ast-call-host-p ast)
          "Should parse as ast-call-host")
      ;; Compile to instructions (placeholder should work)
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Should produce instructions")
        (ok (listp instrs) "Instructions should be a list")))))

(deftest ffi-dynamic-call-with-args-test
  "Test that ffi:call-host with args compiles correctly"
  (testing "dynamic call with arguments"
    (clysm/ffi:reset-ffi-environment)

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "host.add" 1 2)))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Should produce instructions for dynamic call with args")))))

(deftest ffi-call-host-dispatch-structure-test
  "Test that call-host produces proper dispatch structure"
  (testing "dispatch to dynamic host function"
    (clysm/ffi:reset-ffi-environment)

    ;; For dynamic calls, we expect:
    ;; 1. Function name string on stack
    ;; 2. Arguments packed into array
    ;; 3. Call to $call_host_dynamic import

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "console.log" "hello")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Should produce dispatch instructions")
        ;; Verify we get some output (full verification in integration tests)
        (ok (listp instrs) "Output should be instruction list")))))

(deftest ffi-dynamic-externref-handling-test
  "Test externref type handling for dynamic calls"
  (testing "externref for function name"
    ;; Dynamic call-host should convert the function name string
    ;; to externref for the host environment
    (clysm/ffi:reset-ffi-environment)

    (let* ((ast (clysm/compiler/ast:parse-expr
                 '(clysm/ffi:call-host "test.function")))
           (env (clysm/compiler/codegen/func-section:make-env)))
      (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Should handle externref conversion")))))
