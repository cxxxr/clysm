;;;; mismatch-test.lisp - Unit tests for mismatch sequence function
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 3: Sequence Comparison Functions (Priority: P2)
;;;;
;;;; HyperSpec: [mismatch](resources/HyperSpec/Body/f_mismat.htm)

(in-package #:clysm/tests/unit/sequences/mismatch)

(deftest mismatch-basic-diff
  "Test mismatch* finds first difference"
  (ok (= (clysm::mismatch* "abcd" "abXd") 2)
      "mismatch should return 2 for 'abcd' vs 'abXd'"))

(deftest mismatch-identical
  "Test mismatch* on identical sequences"
  (ok (null (clysm::mismatch* "abcd" "abcd"))
      "mismatch should return NIL for identical sequences"))

(deftest mismatch-list
  "Test mismatch* on lists"
  (ok (= (clysm::mismatch* '(a b c) '(a b x)) 2)
      "mismatch should return 2"))

(deftest mismatch-different-lengths
  "Test mismatch* when lengths differ"
  (ok (= (clysm::mismatch* '(a b c) '(a b c d e)) 3)
      "mismatch should return 3 where shorter ends"))

(deftest mismatch-with-start
  "Test mismatch* with :start1/:start2"
  (ok (= (clysm::mismatch* "XXabcd" "abcd" :start1 2) 2)
      "mismatch with start offset"))

(deftest mismatch-empty
  "Test mismatch* with empty sequences"
  (ok (null (clysm::mismatch* "" ""))
      "mismatch of empty strings should be NIL"))
