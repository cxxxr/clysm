;;;; substitute-test.lisp - Unit tests for substitute with keyword args
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US6 - Sequence Functions

(in-package #:clysm/tests)

(deftest substitute-basic-test
  "Test basic substitute compilation."
  (testing "substitute with 3 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(substitute 'x 'a '(a b a c)))))
      (ok wasm "Should compile basic substitute"))))

(deftest substitute-test-keyword-test
  "Test substitute with :test keyword."
  (testing "substitute with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(substitute "x" "a" '("a" "b") :test #'equal))))
      (ok wasm "Should compile substitute with :test"))))

(deftest substitute-count-test
  "Test substitute with :count keyword."
  (testing "substitute with :count compiles"
    (let ((wasm (clysm:compile-to-wasm '(substitute 'x 'a '(a b a c) :count 1))))
      (ok wasm "Should compile substitute with :count"))))
