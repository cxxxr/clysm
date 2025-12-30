;;;; replace-test.lisp - Unit tests for replace sequence function
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 6: Sequence Modification Functions (Priority: P3)
;;;;
;;;; HyperSpec: [replace](resources/HyperSpec/Body/f_replac.htm)

(in-package #:clysm/tests/unit/sequences/replace)

(deftest replace-basic
  "Test replace* basic functionality"
  (let ((seq1 (list 'a 'b 'c 'd))
        (seq2 '(1 2 3)))
    (clysm::replace* seq1 seq2)
    (ok (equal seq1 '(1 2 3 d))
        "replace should copy elements")))

(deftest replace-with-bounds
  "Test replace* with :start1 and :end1"
  (let ((seq1 (list 'a 'b 'c 'd 'e))
        (seq2 '(x y z)))
    (clysm::replace* seq1 seq2 :start1 1 :end1 3)
    (ok (equal seq1 '(a x y d e))
        "replace should respect bounds")))

(deftest replace-vector
  "Test replace* on vectors"
  (let ((vec1 (vector 1 2 3 4 5))
        (vec2 #(a b c)))
    (clysm::replace* vec1 vec2)
    (ok (equalp vec1 #(a b c 4 5))
        "replace should work on vectors")))

(deftest replace-returns-sequence
  "Test replace* returns first sequence"
  (let* ((seq1 (list 1 2 3))
         (seq2 '(a b))
         (result (clysm::replace* seq1 seq2)))
    (ok (eq result seq1)
        "replace should return first sequence")))

(deftest replace-truncates
  "Test replace* stops at shorter range"
  (let ((seq1 (list 'a 'b))
        (seq2 '(1 2 3 4 5)))
    (clysm::replace* seq1 seq2)
    (ok (equal seq1 '(1 2))
        "replace should only copy what fits")))
