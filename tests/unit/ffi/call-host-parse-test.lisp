;;;; call-host-parse-test.lisp - Unit tests for ffi:call-host parsing
;;;; Feature: 027-complete-ffi (T042)

(in-package #:clysm/tests)

(deftest call-host-parsing-test
  "Test that (ffi:call-host ...) parses to ast-call-host"
  (testing "basic call-host parsing"
    ;; Parse a call-host form
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "host.random"))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (let ((func-name (clysm/compiler/ast:ast-call-host-function-name ast)))
        (ok (typep func-name 'clysm/compiler/ast:ast-literal)
            "Function name should be a literal")
        (ok (string= (clysm/compiler/ast:ast-literal-value func-name) "host.random")
            "Function name should be 'host.random'"))))

  (testing "call-host with arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "host.add" 1 2))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (ok (= 2 (length (clysm/compiler/ast:ast-call-host-arguments ast)))
          "Should have 2 arguments"))))

(deftest call-host-dynamic-name-test
  "Test call-host with dynamic function name"
  (testing "variable function name"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host func-name-var 42))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (let ((func-name (clysm/compiler/ast:ast-call-host-function-name ast)))
        ;; The function name is a variable reference
        (ok (typep func-name 'clysm/compiler/ast:ast-var-ref)
            "Function name should be a variable reference")))))
