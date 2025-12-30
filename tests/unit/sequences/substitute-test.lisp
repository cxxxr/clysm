;;;; substitute-test.lisp - Unit tests for substitute sequence functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 4: Element Substitution Functions (Priority: P2)
;;;;
;;;; HyperSpec: [substitute](resources/HyperSpec/Body/f_substc.htm)

(in-package #:clysm/tests/unit/sequences/substitute)

(deftest substitute-basic
  "Test substitute* basic functionality"
  (let ((result (clysm::substitute* 9 1 '(1 2 1 3 1))))
    (ok (equal result '(9 2 9 3 9))
        "substitute 9 for 1 should work")))

(deftest substitute-non-destructive
  "Test substitute* doesn't modify original"
  (let* ((original '(1 2 1 3))
         (result (clysm::substitute* 9 1 original)))
    (ok (equal original '(1 2 1 3))
        "original should be unchanged")
    (ok (equal result '(9 2 9 3))
        "result should have substitutions")))

(deftest substitute-string
  "Test substitute* on string"
  (let ((result (clysm::substitute* #\x #\l "hello")))
    (ok (equal result "hexxo")
        "substitute x for l in hello")))

(deftest substitute-if-basic
  "Test substitute-if*"
  (let ((result (clysm::substitute-if* 0 #'oddp '(1 2 3 4))))
    (ok (equal result '(0 2 0 4))
        "substitute-if should replace odd numbers")))

(deftest substitute-with-count
  "Test substitute* with :count"
  (let ((result (clysm::substitute* 9 1 '(1 2 1 3 1) :count 2)))
    (ok (equal result '(9 2 9 3 1))
        "should only replace first 2")))

(deftest substitute-from-end-count
  "Test substitute* with :from-end and :count"
  (let ((result (clysm::substitute* 9 1 '(1 2 1 3 1) :from-end t :count 2)))
    (ok (equal result '(1 2 9 3 9))
        "should replace last 2")))

(deftest nsubstitute-basic
  "Test nsubstitute* is destructive"
  (let ((vec (vector 1 2 1 3)))
    (clysm::nsubstitute* 9 1 vec)
    (ok (equalp vec #(9 2 9 3))
        "nsubstitute should modify vector")))
