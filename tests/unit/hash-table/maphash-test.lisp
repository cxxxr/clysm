;;;; maphash-test.lisp - Unit tests for maphash compilation
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US4 - Hash Tables

(in-package #:clysm/tests)

(deftest maphash-ast-test
  "Test parsing of maphash forms."
  (testing "maphash parses correctly"
    (let ((ast (clysm/compiler::parse-form '(maphash #'print ht))))
      (ok (clysm/compiler::ast-call-p ast))
      (ok (eq 'maphash (clysm/compiler::ast-call-name ast))))))

(deftest maphash-wasm-test
  "Test Wasm generation for maphash."
  (testing "maphash generates valid Wasm"
    (let ((wasm (clysm:compile-to-wasm '(let ((ht (make-hash-table)))
                                           (maphash (lambda (k v) nil) ht)))))
      (ok wasm "Should generate Wasm bytes"))))
