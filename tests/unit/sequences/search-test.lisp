;;;; search-test.lisp - Unit tests for search sequence function
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 3: Sequence Comparison Functions (Priority: P2)
;;;;
;;;; HyperSpec: [search](resources/HyperSpec/Body/f_search.htm)

(in-package #:clysm/tests/unit/sequences/search)

(deftest search-basic-found
  "Test search* finds subsequence"
  (ok (= (clysm::search* '(a b c) '(a b c d e)) 0)
      "search should return 0"))

(deftest search-middle
  "Test search* finds in middle"
  (ok (= (clysm::search* '(b c) '(a b c d)) 1)
      "search for (b c) should return 1"))

(deftest search-not-found
  "Test search* when not found"
  (ok (null (clysm::search* '(x y) '(a b c)))
      "search should return NIL when not found"))

(deftest search-string
  "Test search* on strings"
  (ok (= (clysm::search* "bc" "abcbc") 1)
      "search 'bc' should return 1"))

(deftest search-from-end
  "Test search* with :from-end"
  (ok (= (clysm::search* "bc" "abcbc" :from-end t) 3)
      "search with from-end should return 3"))

(deftest search-empty-needle
  "Test search* with empty needle"
  (ok (= (clysm::search* "" "abc") 0)
      "search empty should return 0"))
