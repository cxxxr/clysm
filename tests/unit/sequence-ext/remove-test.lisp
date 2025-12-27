;;;; remove-test.lisp - Unit tests for remove with keyword args
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US6 - Sequence Functions

(in-package #:clysm/tests)

(deftest remove-basic-test
  "Test basic remove compilation."
  (testing "remove with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(remove 'a '(a b a c)))))
      (ok wasm "Should compile basic remove"))))

(deftest remove-test-keyword-test
  "Test remove with :test keyword."
  (testing "remove with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(remove "a" '("a" "b") :test #'equal))))
      (ok wasm "Should compile remove with :test"))))

(deftest remove-count-test
  "Test remove with :count keyword."
  (testing "remove with :count compiles"
    (let ((wasm (clysm:compile-to-wasm '(remove 'a '(a b a c) :count 1))))
      (ok wasm "Should compile remove with :count"))))
