;;;; position-test.lisp - Unit tests for position sequence functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 2: Element Search Functions (Priority: P1)
;;;;
;;;; HyperSpec: [position](resources/HyperSpec/Body/f_pos_p.htm)
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(in-package #:clysm/tests/unit/sequences/position)

;;; ============================================================
;;; T030: position* Basic Functionality Tests
;;; ============================================================

(deftest position-basic-list
  "Test position* on list"
  (ok (= (clysm::position* 3 '(1 2 3 4)) 2)
      "position 3 in (1 2 3 4) should return 2"))

(deftest position-not-found
  "Test position* when element not found"
  (ok (null (clysm::position* 'z '(a b c d)))
      "position z in (a b c d) should return NIL"))

(deftest position-basic-vector
  "Test position* on vector"
  (ok (= (clysm::position* 30 #(10 20 30 40)) 2)
      "position 30 in vector should return 2"))

(deftest position-basic-string
  "Test position* on string"
  (ok (= (clysm::position* #\l "hello") 2)
      "position #\\l in \"hello\" should return 2"))

;;; ============================================================
;;; T031: position* with :test, :key, :from-end Tests
;;; ============================================================

(deftest position-with-test-equal
  "Test position* with :test #'equal"
  (ok (= (clysm::position* "banana" '("apple" "banana" "cherry") :test #'equal)
         1)
      "position with equal should return 1"))

(deftest position-with-key
  "Test position* with :key"
  (ok (= (clysm::position* 'b '((a 1) (b 2) (c 3)) :key #'car)
         1)
      "position with :key #'car should return 1"))

(deftest position-from-end
  "Test position* with :from-end"
  (ok (= (clysm::position* 3 '(1 2 3 4 3 2 1) :from-end t)
         4)
      "position 3 with :from-end should return 4"))

(deftest position-from-end-first-match
  "Test position* :from-end returns index from start"
  ;; Important: position is still 0-indexed from start, even with :from-end
  (ok (= (clysm::position* 'a '(a b a c a) :from-end t)
         4)
      "position a with :from-end should return 4 (last a's index from start)"))

;;; ============================================================
;;; T032: position-if* and position-if-not* Tests
;;; ============================================================

(deftest position-if-evenp
  "Test position-if* with evenp"
  (ok (= (clysm::position-if* #'evenp '(1 2 3 4 5)) 1)
      "position-if evenp should return 1 (index of 2)"))

(deftest position-if-not-found
  "Test position-if* when no element satisfies"
  (ok (null (clysm::position-if* #'evenp '(1 3 5 7 9)))
      "position-if evenp in odd list should return NIL"))

(deftest position-if-not-oddp
  "Test position-if-not* with oddp"
  (ok (= (clysm::position-if-not* #'oddp '(1 2 3 4 5)) 1)
      "position-if-not oddp should return 1"))

(deftest position-if-with-key
  "Test position-if* with :key"
  (ok (= (clysm::position-if* #'evenp '((a 1) (b 2) (c 3)) :key #'cadr)
         1)
      "position-if with :key should return index 1"))

(deftest position-if-from-end
  "Test position-if* with :from-end"
  (ok (= (clysm::position-if* #'evenp '(1 2 3 4 5) :from-end t)
         3)
      "position-if evenp with :from-end should return 3 (index of 4)"))

;;; ============================================================
;;; T033: Edge Cases
;;; ============================================================

(deftest position-empty-list
  "Test position* on empty list"
  (ok (null (clysm::position* 'x '()))
      "position in empty list should return NIL"))

(deftest position-empty-vector
  "Test position* on empty vector"
  (ok (null (clysm::position* 1 #()))
      "position in empty vector should return NIL"))

(deftest position-with-start
  "Test position* with :start"
  (ok (= (clysm::position* 1 '(1 2 1 3 1) :start 1) 2)
      "position 1 starting from index 1 should return 2"))

(deftest position-with-end
  "Test position* with :end"
  (ok (null (clysm::position* 3 '(1 2 3 4 5) :end 2))
      "position 3 ending at index 2 should return NIL"))

(deftest position-with-start-end
  "Test position* with :start and :end bounds"
  (ok (= (clysm::position* 2 '(1 2 3 2 5) :start 2 :end 4) 3)
      "position 2 in range [2,4) should return 3"))

(deftest position-first-element
  "Test position* finds first element at index 0"
  (ok (= (clysm::position* 'a '(a b c d)) 0)
      "position of first element should be 0"))

(deftest position-last-element
  "Test position* finds last element"
  (ok (= (clysm::position* 'd '(a b c d)) 3)
      "position of last element should be 3"))
