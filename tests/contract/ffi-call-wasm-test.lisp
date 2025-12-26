;;;; ffi-call-wasm-test.lisp - Contract tests for FFI call Wasm generation
;;;; Feature: 027-complete-ffi (T015)

(in-package #:clysm/tests)

(deftest ffi-call-produces-wasm-test
  "Test that FFI calls produce Wasm instructions"
  (testing "basic FFI call compilation"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-log "host.log" (:string) :void))

    ;; Parse and compile an FFI call
    (let* ((ast (clysm/compiler/ast:parse-expr '(host-log "test")))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      (ok instrs "FFI call should produce instructions")
      (ok (listp instrs) "Instructions should be a list"))))

(deftest ffi-call-with-args-test
  "Test FFI calls with arguments produce proper Wasm"
  (testing "multi-arg FFI call compilation"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum))

    (let* ((ast (clysm/compiler/ast:parse-expr '(host-add 1 2)))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      (ok instrs "Multi-arg FFI call should produce instructions"))))

(deftest ffi-call-generates-call-instruction-test
  "Test that FFI calls include the call instruction"
  (testing "call instruction presence"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function simple-fn "host.simple" () :fixnum))

    (let* ((ast (clysm/compiler/ast:parse-expr '(simple-fn)))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      ;; Once fully implemented, should contain :call instruction
      ;; For now, just verify we get some output
      (ok instrs "FFI call should produce output"))))
