;;;; assoc-test.lisp - Unit tests for assoc with :test/:key
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US5 - List Functions

(in-package #:clysm/tests)

(deftest assoc-basic-test
  "Test basic assoc compilation."
  (testing "assoc with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(assoc 'a '((a . 1) (b . 2))))))
      (ok wasm "Should compile basic assoc"))))

(deftest assoc-test-keyword-test
  "Test assoc with :test keyword."
  (testing "assoc with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(assoc "a" '(("a" . 1)) :test #'equal))))
      (ok wasm "Should compile assoc with :test"))))

(deftest assoc-key-keyword-test
  "Test assoc with :key keyword."
  (testing "assoc with :key #'car compiles"
    (let ((wasm (clysm:compile-to-wasm '(assoc 1 '(((1 . a) . x)) :key #'caar))))
      (ok wasm "Should compile assoc with :key"))))
