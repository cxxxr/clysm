;;;; call-host-args-test.lisp - Unit tests for ffi:call-host argument handling
;;;; Feature: 027-complete-ffi (T043)

(in-package #:clysm/tests)

(deftest call-host-args-compilation-test
  "Test that call-host arguments are compiled correctly"
  (testing "no arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "host.random"))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (ok (null (clysm/compiler/ast:ast-call-host-arguments ast))
          "Should have no arguments")))

  (testing "single argument"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "host.log" "message"))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (ok (= 1 (length (clysm/compiler/ast:ast-call-host-arguments ast)))
          "Should have 1 argument")))

  (testing "multiple arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "host.add" 1 2 3))))
      (ok (typep ast 'clysm/compiler/ast:ast-call-host)
          "Should parse as ast-call-host")
      (ok (= 3 (length (clysm/compiler/ast:ast-call-host-arguments ast)))
          "Should have 3 arguments"))))

(deftest call-host-argument-types-test
  "Test that call-host handles various argument types"
  (testing "literal arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "fn" 42 "str" t))))
      (let ((args (clysm/compiler/ast:ast-call-host-arguments ast)))
        (ok (= 3 (length args)) "Should have 3 arguments")
        ;; First arg should be a literal (fixnum)
        (ok (typep (first args) 'clysm/compiler/ast:ast-literal)
            "First arg should be a literal")
        ;; Second arg should be a literal (string)
        (ok (typep (second args) 'clysm/compiler/ast:ast-literal)
            "Second arg should be a literal")
        ;; Third arg should be a literal (boolean)
        (ok (typep (third args) 'clysm/compiler/ast:ast-literal)
            "Third arg should be a literal"))))

  (testing "variable arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "fn" x y))))
      (let ((args (clysm/compiler/ast:ast-call-host-arguments ast)))
        (ok (= 2 (length args)) "Should have 2 arguments")
        (ok (typep (first args) 'clysm/compiler/ast:ast-var-ref)
            "First arg should be a variable reference")
        (ok (typep (second args) 'clysm/compiler/ast:ast-var-ref)
            "Second arg should be a variable reference"))))

  (testing "expression arguments"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "fn" (+ 1 2) (* 3 4)))))
      (let ((args (clysm/compiler/ast:ast-call-host-arguments ast)))
        (ok (= 2 (length args)) "Should have 2 arguments")
        (ok (typep (first args) 'clysm/compiler/ast:ast-call)
            "First arg should be a call expression")
        (ok (typep (second args) 'clysm/compiler/ast:ast-call)
            "Second arg should be a call expression")))))

(deftest call-host-function-name-types-test
  "Test that call-host accepts various function name types"
  (testing "string literal function name"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host "console.log" 42))))
      (let ((fname (clysm/compiler/ast:ast-call-host-function-name ast)))
        (ok (typep fname 'clysm/compiler/ast:ast-literal)
            "Function name should be a literal")
        (ok (string= "console.log" (clysm/compiler/ast:ast-literal-value fname))
            "Function name should be correct"))))

  (testing "variable function name"
    (let ((ast (clysm/compiler/ast:parse-expr
                '(clysm/ffi:call-host fn-name 42))))
      (let ((fname (clysm/compiler/ast:ast-call-host-function-name ast)))
        (ok (typep fname 'clysm/compiler/ast:ast-var-ref)
            "Function name should be a variable reference")
        (ok (eq 'fn-name (clysm/compiler/ast:ast-var-ref-name fname))
            "Variable name should be correct")))))
