;;;; position-test.lisp - Unit tests for position with keyword args
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US6 - Sequence Functions

(in-package #:clysm/tests)

(deftest position-basic-test
  "Test basic position compilation."
  (testing "position with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(position 'a '(a b c)))))
      (ok wasm "Should compile basic position"))))

(deftest position-test-keyword-test
  "Test position with :test keyword."
  (testing "position with :test #'equal compiles"
    (let ((wasm (clysm:compile-to-wasm '(position "a" '("a" "b") :test #'equal))))
      (ok wasm "Should compile position with :test"))))

(deftest position-range-test
  "Test position with :start/:end keywords."
  (testing "position with :start compiles"
    (let ((wasm (clysm:compile-to-wasm '(position 'b '(a b c) :start 1))))
      (ok wasm "Should compile position with :start")))

  (testing "position with :end compiles"
    (let ((wasm (clysm:compile-to-wasm '(position 'b '(a b c) :end 2))))
      (ok wasm "Should compile position with :end"))))

(deftest position-from-end-test
  "Test position with :from-end keyword."
  (testing "position with :from-end compiles"
    (let ((wasm (clysm:compile-to-wasm '(position 'a '(a b a) :from-end t))))
      (ok wasm "Should compile position with :from-end"))))
