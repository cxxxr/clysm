;;;; remhash-test.lisp - Unit tests for remhash compilation
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US4 - Hash Tables

(in-package #:clysm/tests)

(deftest remhash-ast-test
  "Test parsing of remhash forms."
  (testing "remhash parses correctly"
    (let ((ast (clysm/compiler::parse-form '(remhash 'key ht))))
      (ok (clysm/compiler::ast-call-p ast))
      (ok (eq 'remhash (clysm/compiler::ast-call-name ast))))))

(deftest remhash-wasm-test
  "Test Wasm generation for remhash."
  (testing "remhash generates valid Wasm"
    (let ((wasm (clysm:compile-to-wasm '(let ((ht (make-hash-table)))
                                           (remhash 'key ht)))))
      (ok wasm "Should generate Wasm bytes"))))
