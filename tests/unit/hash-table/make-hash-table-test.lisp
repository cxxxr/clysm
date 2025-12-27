;;;; make-hash-table-test.lisp - Unit tests for make-hash-table compilation
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US4 - Hash Tables

(in-package #:clysm/tests)

(deftest make-hash-table-ast-test
  "Test parsing of make-hash-table forms."
  (testing "basic make-hash-table parses correctly"
    (let ((ast (clysm/compiler::parse-form '(make-hash-table))))
      (ok (clysm/compiler::ast-make-hash-table-p ast))))

  (testing "make-hash-table with :test parses correctly"
    (let ((ast (clysm/compiler::parse-form '(make-hash-table :test #'equal))))
      (ok (clysm/compiler::ast-make-hash-table-p ast))
      (ok (clysm/compiler::ast-make-hash-table-test ast))))

  (testing "make-hash-table with :size parses correctly"
    (let ((ast (clysm/compiler::parse-form '(make-hash-table :size 100))))
      (ok (clysm/compiler::ast-make-hash-table-p ast))
      (ok (clysm/compiler::ast-make-hash-table-size ast)))))

(deftest make-hash-table-wasm-test
  "Test Wasm generation for make-hash-table."
  (testing "basic make-hash-table generates valid Wasm"
    (let ((wasm (clysm:compile-to-wasm '(make-hash-table))))
      (ok wasm "Should generate Wasm bytes")
      (ok (clysm/tests::valid-wasm-p wasm) "Should be valid Wasm"))))
