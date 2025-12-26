;;;; ffi-call-parse-test.lisp - Unit tests for FFI call AST parsing
;;;; Feature: 027-complete-ffi (T012)

(in-package #:clysm/tests)

(deftest ffi-call-ast-parsing-test
  "Test that calls to FFI-declared functions produce ast-ffi-call nodes"
  (testing "FFI function call parsing"
    ;; Reset and register an FFI function
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function console-log "host.log" (:string) :void))

    ;; Parse a call to the FFI function
    (let ((ast (clysm/compiler/ast:parse-expr '(console-log "hello"))))
      ;; Should produce ast-ffi-call, not ast-call
      (ok (typep ast 'clysm/compiler/ast:ast-ffi-call)
          "Call to FFI function should produce ast-ffi-call")
      (ok (clysm/compiler/ast:ast-ffi-call-declaration ast)
          "ast-ffi-call should have a declaration")
      (ok (= 1 (length (clysm/compiler/ast:ast-ffi-call-arguments ast)))
          "ast-ffi-call should have 1 argument"))))

(deftest regular-call-not-ffi-test
  "Test that calls to non-FFI functions produce regular ast-call nodes"
  (testing "non-FFI function call parsing"
    (clysm/ffi:reset-ffi-environment)

    ;; Parse a call to a regular function (not in FFI environment)
    (let ((ast (clysm/compiler/ast:parse-expr '(my-regular-fn 1 2 3))))
      ;; Should produce ast-call, not ast-ffi-call
      (ok (typep ast 'clysm/compiler/ast:ast-call)
          "Call to non-FFI function should produce ast-call")
      (ok (eq (clysm/compiler/ast:ast-call-function ast) 'my-regular-fn)
          "Function name should be preserved")
      (ok (= 3 (length (clysm/compiler/ast:ast-call-arguments ast)))
          "ast-call should have 3 arguments"))))

(deftest ffi-call-with-multiple-args-test
  "Test FFI call parsing with multiple arguments"
  (testing "multiple argument parsing"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum))

    (let ((ast (clysm/compiler/ast:parse-expr '(host-add 40 2))))
      (ok (typep ast 'clysm/compiler/ast:ast-ffi-call)
          "Should be ast-ffi-call")
      (let ((args (clysm/compiler/ast:ast-ffi-call-arguments ast)))
        (ok (= 2 (length args)) "Should have 2 arguments")
        ;; Check that arguments are parsed literals
        (ok (typep (first args) 'clysm/compiler/ast:ast-literal)
            "First arg should be a literal")
        (ok (typep (second args) 'clysm/compiler/ast:ast-literal)
            "Second arg should be a literal")))))

(deftest ffi-call-declaration-reference-test
  "Test that ast-ffi-call references the correct declaration"
  (testing "declaration reference"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function my-ffi "host.fn" (:anyref) :anyref))

    (let* ((ast (clysm/compiler/ast:parse-expr '(my-ffi x)))
           (decl (clysm/compiler/ast:ast-ffi-call-declaration ast)))
      (ok decl "Declaration should be present")
      (ok (eq (clysm/ffi:ffd-lisp-name decl) 'my-ffi)
          "Declaration should have correct lisp-name")
      (ok (string= (clysm/ffi:ffd-module-name decl) "host")
          "Declaration should have correct module-name"))))
