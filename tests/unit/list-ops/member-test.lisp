;;;; member-test.lisp - Unit tests for member with :test/:key
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US5 - List Functions

(in-package #:clysm/tests)

(deftest member-basic-test
  "Test basic member compilation."
  (testing "member with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(member 'a '(a b c)))))
      (ok wasm "Should compile basic member"))))

(deftest member-test-keyword-test
  "Test member with :test keyword."
  (testing "member with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(member "a" '("a" "b") :test #'equal))))
      (ok wasm "Should compile member with :test"))))

(deftest member-key-keyword-test
  "Test member with :key keyword."
  (testing "member with :key #'car compiles"
    (let ((wasm (clysm:compile-to-wasm '(member 1 '((1 . a) (2 . b)) :key #'car))))
      (ok wasm "Should compile member with :key"))))
