;;;; set-ops-test.lisp - Unit tests for union/intersection/adjoin
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US5 - List Functions

(in-package #:clysm/tests)

(deftest union-test
  "Test union compilation."
  (testing "basic union compiles"
    (let ((wasm (clysm:compile-to-wasm '(union '(1 2) '(2 3)))))
      (ok wasm "Should compile union")))

  (testing "union with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(union '("a") '("b") :test #'equal))))
      (ok wasm "Should compile union with :test"))))

(deftest intersection-test
  "Test intersection compilation."
  (testing "basic intersection compiles"
    (let ((wasm (clysm:compile-to-wasm '(intersection '(1 2 3) '(2 3 4)))))
      (ok wasm "Should compile intersection")))

  (testing "intersection with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(intersection '("a") '("a") :test #'equal))))
      (ok wasm "Should compile intersection with :test"))))

(deftest adjoin-test
  "Test adjoin compilation."
  (testing "basic adjoin compiles"
    (let ((wasm (clysm:compile-to-wasm '(adjoin 1 '(2 3)))))
      (ok wasm "Should compile adjoin")))

  (testing "adjoin with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(adjoin "a" '("b") :test #'equal))))
      (ok wasm "Should compile adjoin with :test"))))
