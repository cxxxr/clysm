;;;; find-test.lisp - Unit tests for find sequence functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 2: Element Search Functions (Priority: P1)
;;;;
;;;; HyperSpec: [find](resources/HyperSpec/Body/f_find_.htm)
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(in-package #:clysm/tests/unit/sequences/find)

;;; ============================================================
;;; T027: find* Basic Functionality Tests
;;; ============================================================

(deftest find-basic-list
  "Test find* on list"
  (ok (eq (clysm::find* 'c '(a b c d)) 'c)
      "find c in (a b c d) should return c"))

(deftest find-not-found
  "Test find* when element not found"
  (ok (null (clysm::find* 'z '(a b c d)))
      "find z in (a b c d) should return NIL"))

(deftest find-basic-vector
  "Test find* on vector"
  (ok (= (clysm::find* 3 #(1 2 3 4 5)) 3)
      "find 3 in vector should return 3"))

(deftest find-basic-string
  "Test find* on string"
  (ok (char= (clysm::find* #\l "hello") #\l)
      "find #\\l in \"hello\" should return #\\l"))

;;; ============================================================
;;; T028: find* with :test, :key, :from-end Tests
;;; ============================================================

(deftest find-with-test-equal
  "Test find* with :test #'equal"
  (ok (equal (clysm::find* "banana" '("apple" "banana" "cherry") :test #'equal)
             "banana")
      "find with equal should find string"))

(deftest find-with-test-string-equal
  "Test find* with :test #'string-equal"
  (ok (equal (clysm::find* "BANANA" '("apple" "banana" "cherry") :test #'string-equal)
             "banana")
      "find with string-equal should find case-insensitive"))

(deftest find-with-key
  "Test find* with :key"
  (ok (equal (clysm::find* 'a '((a 1) (b 2) (c 3)) :key #'car)
             '(a 1))
      "find with :key #'car should return full entry"))

(deftest find-from-end
  "Test find* with :from-end"
  ;; With from-end, should find the last matching element
  (ok (equal (clysm::find* 'a '((a 1) (b 2) (a 3)) :key #'car :from-end t)
             '(a 3))
      "find with :from-end should return last match"))

;;; ============================================================
;;; T029: find-if* and find-if-not* Tests
;;; ============================================================

(deftest find-if-evenp
  "Test find-if* with evenp"
  (ok (= (clysm::find-if* #'evenp '(1 2 3 4 5)) 2)
      "find-if evenp should return 2"))

(deftest find-if-not-found
  "Test find-if* when no element satisfies"
  (ok (null (clysm::find-if* #'evenp '(1 3 5 7 9)))
      "find-if evenp in odd list should return NIL"))

(deftest find-if-not-oddp
  "Test find-if-not* with oddp"
  (ok (= (clysm::find-if-not* #'oddp '(1 2 3 4 5)) 2)
      "find-if-not oddp should return 2 (first even)"))

(deftest find-if-with-key
  "Test find-if* with :key"
  (ok (equal (clysm::find-if* #'evenp '((a 1) (b 2) (c 3)) :key #'cadr)
             '(b 2))
      "find-if with :key #'cadr should find entry with even value"))

(deftest find-if-from-end
  "Test find-if* with :from-end"
  (ok (equal (clysm::find-if* #'evenp '(1 2 3 4 5) :from-end t)
             4)
      "find-if with :from-end should return 4 (last even)"))

;;; ============================================================
;;; T033: Edge Cases
;;; ============================================================

(deftest find-empty-list
  "Test find* on empty list"
  (ok (null (clysm::find* 'x '()))
      "find in empty list should return NIL"))

(deftest find-empty-vector
  "Test find* on empty vector"
  (ok (null (clysm::find* 1 #()))
      "find in empty vector should return NIL"))

(deftest find-with-start
  "Test find* with :start"
  (ok (= (clysm::find* 1 '(1 2 1 3 1) :start 2) 1)
      "find 1 starting from index 2 should still return 1"))

(deftest find-with-end
  "Test find* with :end"
  (ok (null (clysm::find* 3 '(1 2 3 4 5) :end 2))
      "find 3 ending at index 2 should return NIL"))

(deftest find-with-start-end
  "Test find* with :start and :end"
  (ok (= (clysm::find* 2 '(1 2 3 2 5) :start 2 :end 4) 2)
      "find 2 in range [2,4) should return 2 at index 3"))
