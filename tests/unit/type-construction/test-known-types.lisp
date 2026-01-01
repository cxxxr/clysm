;;;; test-known-types.lisp - Unit tests for known type index lookup
;;;; Task: T041 [US4]
;;;;
;;;; TDD: Tests written FIRST before implementation.
;;;; These tests must FAIL initially (T043).

(in-package #:clysm/tests)

(deftest test-type-index-for-cons ()
  "Test that cons type returns correct index (0)."
  (ok (= (clysm::type-index-for-name 'cons) 0)
      "cons should have type index 0"))

(deftest test-type-index-for-symbol ()
  "Test that symbol type returns correct index (1)."
  (ok (= (clysm::type-index-for-name 'symbol) 1)
      "symbol should have type index 1"))

(deftest test-type-index-for-closure ()
  "Test that closure type returns correct index (3)."
  (ok (= (clysm::type-index-for-name 'closure) 3)
      "closure should have type index 3"))

(deftest test-type-index-for-instance ()
  "Test that instance type returns correct index (6)."
  (ok (= (clysm::type-index-for-name 'instance) 6)
      "instance should have type index 6"))

(deftest test-type-index-for-hash-table ()
  "Test that hash-table type returns correct index (18)."
  (ok (= (clysm::type-index-for-name 'hash-table) 18)
      "hash-table should have type index 18"))

(deftest test-type-index-string-name ()
  "Test that string names work for lookup."
  (ok (= (clysm::type-index-for-name "CONS") 0)
      "String CONS should resolve to index 0"))

(deftest test-all-known-types ()
  "Test all 29 pre-allocated type indices."
  (let ((expected-types '((cons . 0)
                          (symbol . 1)
                          (string . 2)
                          (closure . 3)
                          (float . 4)
                          (ratio . 5)
                          (instance . 6)
                          (standard-class . 7)
                          (hash-table . 18)
                          (mdarray . 28))))
    (dolist (entry expected-types)
      (ok (= (clysm::type-index-for-name (car entry)) (cdr entry))
          (format nil "~A should have index ~D" (car entry) (cdr entry))))))
