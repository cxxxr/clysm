;;;; find-test.lisp - Unit tests for find with keyword args
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US6 - Sequence Functions

(in-package #:clysm/tests)

(deftest find-basic-test
  "Test basic find compilation."
  (testing "find with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(find 'a '(a b c)))))
      (ok wasm "Should compile basic find"))))

(deftest find-test-keyword-test
  "Test find with :test keyword."
  (testing "find with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(find "a" '("a" "b") :test #'equal))))
      (ok wasm "Should compile find with :test"))))

(deftest find-key-keyword-test
  "Test find with :key keyword."
  (testing "find with :key #'car compiles"
    (let ((wasm (clysm:compile-to-wasm '(find 1 '((1 . a) (2 . b)) :key #'car))))
      (ok wasm "Should compile find with :key"))))
