;;;; count-test.lisp - Unit tests for count sequence functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 1: Element Counting Functions (Priority: P1)
;;;;
;;;; HyperSpec: [count](resources/HyperSpec/Body/f_countc.htm)
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(in-package #:clysm/tests/unit/sequences/count)

;;; ============================================================
;;; T011: count* Basic Functionality Tests
;;; ============================================================

(deftest count-basic-list
  "Test count* on list"
  (ok (= (clysm::count* 1 '(1 2 1 3 1 4)) 3)
      "count 1 in (1 2 1 3 1 4) should return 3"))

(deftest count-basic-vector
  "Test count* on vector"
  (ok (= (clysm::count* 'b #(a b c b d b)) 3)
      "count b in #(a b c b d b) should return 3"))

(deftest count-basic-string
  "Test count* on string"
  (ok (= (clysm::count* #\l "hello") 2)
      "count #\\l in \"hello\" should return 2"))

(deftest count-not-found
  "Test count* when element not found"
  (ok (= (clysm::count* 'z '(a b c d)) 0)
      "count z in (a b c d) should return 0"))

;;; ============================================================
;;; T012: count* with :key Keyword Tests
;;; ============================================================

(deftest count-with-key-car
  "Test count* with :key #'car"
  (ok (= (clysm::count* 'a '((a 1) (b 2) (a 3)) :key #'car) 2)
      "count a in alist by car should return 2"))

(deftest count-with-key-cadr
  "Test count* with :key #'cadr"
  (ok (= (clysm::count* 2 '((a 1) (b 2) (c 2)) :key #'cadr) 2)
      "count 2 in alist by cadr should return 2"))

;;; ============================================================
;;; T013: count* with :test Keyword Tests
;;; ============================================================

(deftest count-with-test-equal
  "Test count* with :test #'equal"
  (ok (= (clysm::count* "hello" '("hello" "world" "hello") :test #'equal) 2)
      "count \"hello\" with equal should return 2"))

(deftest count-with-test-string-equal
  "Test count* with :test #'string-equal (case-insensitive)"
  (ok (= (clysm::count* "HELLO" '("hello" "world" "HELLO") :test #'string-equal) 2)
      "count case-insensitive should return 2"))

(deftest count-with-test-eq
  "Test count* with :test #'eq"
  (let ((sym 'foo))
    (ok (= (clysm::count* sym (list sym 'bar sym) :test #'eq) 2)
        "count with eq should find identical symbols")))

;;; ============================================================
;;; T014: count* with :start/:end Bounds Tests
;;; ============================================================

(deftest count-with-start
  "Test count* with :start"
  (ok (= (clysm::count* 1 '(1 1 1 1 1) :start 2) 3)
      "count 1 starting from index 2 should return 3"))

(deftest count-with-end
  "Test count* with :end"
  (ok (= (clysm::count* 1 '(1 1 1 1 1) :end 3) 3)
      "count 1 ending at index 3 should return 3"))

(deftest count-with-start-end
  "Test count* with both :start and :end"
  (ok (= (clysm::count* 1 '(1 2 1 2 1 2 1) :start 1 :end 5) 2)
      "count 1 in range [1,5) should return 2"))

(deftest count-with-start-end-list
  "Test count-if with :start/:end per spec"
  (ok (= (clysm::count-if* #'evenp '(1 2 3 4 5) :start 1 :end 4) 2)
      "count-if evenp in [1,4) should return 2 (elements: 2, 4)"))

;;; ============================================================
;;; T015: count* with :from-end Tests
;;; ============================================================

(deftest count-from-end-no-effect
  "Test count* with :from-end (should not affect count)"
  ;; :from-end doesn't affect count since we're just counting
  (ok (= (clysm::count* 1 '(1 2 1 3 1) :from-end t) 3)
      "count with :from-end should still return 3"))

(deftest count-from-end-with-bounds
  "Test count* :from-end with bounds"
  (ok (= (clysm::count* 1 '(1 2 1 3 1 4 1) :start 1 :end 5 :from-end t) 2)
      "count in range [1,5) with :from-end should return 2"))

;;; ============================================================
;;; T016: count-if* and count-if-not* Tests
;;; ============================================================

(deftest count-if-evenp
  "Test count-if* with evenp"
  (ok (= (clysm::count-if* #'evenp '(1 2 3 4 5)) 2)
      "count-if evenp should return 2"))

(deftest count-if-oddp
  "Test count-if* with oddp"
  (ok (= (clysm::count-if* #'oddp '(1 2 3 4 5)) 3)
      "count-if oddp should return 3"))

(deftest count-if-not-evenp
  "Test count-if-not* with evenp"
  (ok (= (clysm::count-if-not* #'evenp '(1 2 3 4 5)) 3)
      "count-if-not evenp should return 3"))

(deftest count-if-not-oddp
  "Test count-if-not* with oddp"
  (ok (= (clysm::count-if-not* #'oddp '(1 2 3 4 5)) 2)
      "count-if-not oddp should return 2"))

(deftest count-if-with-key
  "Test count-if* with :key"
  (ok (= (clysm::count-if* #'evenp '((a 1) (b 2) (c 3) (d 4)) :key #'cadr) 2)
      "count-if evenp on cadr should return 2"))

(deftest count-if-string
  "Test count-if* on string"
  (ok (= (clysm::count-if* #'upper-case-p "Hello World") 2)
      "count-if upper-case-p should return 2"))

(deftest count-if-vector
  "Test count-if* on vector"
  (ok (= (clysm::count-if* #'numberp #(1 a 2 b 3)) 3)
      "count-if numberp in vector should return 3"))

;;; ============================================================
;;; T017: Edge Case Tests
;;; ============================================================

(deftest count-empty-list
  "Test count* on empty list"
  (ok (= (clysm::count* 'x '()) 0)
      "count in empty list should return 0"))

(deftest count-empty-vector
  "Test count* on empty vector"
  (ok (= (clysm::count* 'x #()) 0)
      "count in empty vector should return 0"))

(deftest count-empty-string
  "Test count* on empty string"
  (ok (= (clysm::count* #\x "") 0)
      "count in empty string should return 0"))

(deftest count-start-equals-end
  "Test count* when start equals end"
  (ok (= (clysm::count* 1 '(1 1 1) :start 1 :end 1) 0)
      "count with start=end should return 0 (empty range)"))

(deftest count-if-empty-list
  "Test count-if* on empty list"
  (ok (= (clysm::count-if* #'evenp '()) 0)
      "count-if on empty list should return 0"))

(deftest count-all-match
  "Test count* when all elements match"
  (ok (= (clysm::count* 1 '(1 1 1 1 1)) 5)
      "count 1 in (1 1 1 1 1) should return 5"))

(deftest count-none-match
  "Test count* when no elements match"
  (ok (= (clysm::count* 'z '(a b c d e)) 0)
      "count z in (a b c d e) should return 0"))
