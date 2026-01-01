;;;; compile-fn-test.lisp - Unit tests for compile-runtime-function
;;;; Feature 001-runtime-library-system
;;;; Task T026: Unit test for compile-runtime-function

(in-package #:clysm/tests)

(deftest compile-runtime-function-uses-existing-compiler ()
  "Verify compile-runtime-function delegates to compile-form"
  (testing "uses existing compilation infrastructure"
    (skip "compile-runtime-function not yet implemented")))

(deftest compile-runtime-function-tracks-index ()
  "Verify compiled-index is set after compilation"
  (testing "function index tracking"
    (skip "compile-runtime-function not yet implemented")))

(deftest compile-runtime-function-returns-wasm ()
  "Verify compilation returns Wasm bytecode"
  (testing "Wasm bytecode output"
    (skip "compile-runtime-function not yet implemented")))
