;;;; fill-test.lisp - Unit tests for fill sequence function
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 6: Sequence Modification Functions (Priority: P3)
;;;;
;;;; HyperSpec: [fill](resources/HyperSpec/Body/f_fill.htm)

(in-package #:clysm/tests/unit/sequences/fill)

(deftest fill-basic-list
  "Test fill* on list"
  (let ((lst (list 'a 'b 'c 'd 'e)))
    (clysm::fill* lst 'x)
    (ok (equal lst '(x x x x x))
        "fill should replace all elements")))

(deftest fill-with-bounds
  "Test fill* with :start and :end"
  (let ((vec (vector 1 2 3 4 5)))
    (clysm::fill* vec 0 :start 1 :end 4)
    (ok (equalp vec #(1 0 0 0 5))
        "fill should only affect bounded range")))

(deftest fill-string
  "Test fill* on string"
  (let ((str (copy-seq "hello")))
    (clysm::fill* str #\x)
    (ok (equal str "xxxxx")
        "fill should replace all chars")))

(deftest fill-empty-range
  "Test fill* with start=end"
  (let ((lst (list 1 2 3)))
    (clysm::fill* lst 'x :start 1 :end 1)
    (ok (equal lst '(1 2 3))
        "fill with empty range should do nothing")))

(deftest fill-returns-sequence
  "Test fill* returns the modified sequence"
  (let* ((lst (list 1 2 3))
         (result (clysm::fill* lst 0)))
    (ok (eq result lst)
        "fill should return the same sequence")))
