;;;; ffi-import-call-test.lisp - Integration tests for FFI import and call
;;;; Feature: 027-complete-ffi (T016)

(in-package #:clysm/tests)

(deftest ffi-import-call-integration-test
  "Test complete FFI import/call workflow from declaration to Wasm"
  (testing "end-to-end FFI import call"
    ;; Reset environment
    (clysm/ffi:reset-ffi-environment)

    ;; Define a foreign function
    (eval '(clysm/ffi:define-foreign-function host-log "host.log" (:string) :void))

    ;; Assign indices
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    ;; Compile a program that calls the FFI function
    (let* ((program '(host-log "Hello from Lisp"))
           (ast (clysm/compiler/ast:parse-expr program)))
      ;; Verify it's an FFI call
      (ok (clysm/compiler/ast:ast-ffi-call-p ast)
          "Should parse as ast-ffi-call")

      ;; Compile to Wasm instructions
      (let* ((env (clysm/compiler/codegen/func-section:make-env))
             (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
        (ok instrs "Should produce Wasm instructions")
        (ok (listp instrs) "Instructions should be a list")))))

(deftest ffi-multi-arg-integration-test
  "Test FFI call with multiple arguments"
  (testing "multi-arg FFI call"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let* ((program '(host-add 40 2))
           (ast (clysm/compiler/ast:parse-expr program))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      (ok instrs "Multi-arg FFI call should produce instructions")
      ;; Should contain marshalling and call
      (ok (find-if (lambda (i) (and (listp i) (eq (car i) :call))) instrs)
          "Should contain :call instruction"))))

(deftest ffi-different-types-integration-test
  "Test FFI calls with different marshal types"
  (testing "float parameter"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-sqrt "host.sqrt" (:float) :float))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    ;; Parse a call - note: 2.0 is a float literal
    (let* ((ast (clysm/compiler/ast:parse-expr '(host-sqrt 2.0)))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      (ok instrs "Float FFI call should produce instructions")))

  (testing "boolean parameter"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-negate "host.negate" (:boolean) :boolean))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let* ((ast (clysm/compiler/ast:parse-expr '(host-negate t)))
           (env (clysm/compiler/codegen/func-section:make-env))
           (instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
      (ok instrs "Boolean FFI call should produce instructions"))))
