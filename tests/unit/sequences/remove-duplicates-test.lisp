;;;; remove-duplicates-test.lisp - Unit tests for remove-duplicates functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; User Story 5: Duplicate Removal Functions (Priority: P2)
;;;;
;;;; HyperSpec: [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)

(in-package #:clysm/tests/unit/sequences/remove-duplicates)

(deftest remove-duplicates-keep-last
  "Test remove-duplicates* keeps last occurrences by default"
  (let ((result (clysm::remove-duplicates* '(a b a c b d))))
    (ok (equal result '(a c b d))
        "should keep last occurrences")))

(deftest remove-duplicates-keep-first
  "Test remove-duplicates* with :from-end keeps first"
  (let ((result (clysm::remove-duplicates* '(a b a c b d) :from-end t)))
    (ok (equal result '(a b c d))
        "should keep first occurrences")))

(deftest remove-duplicates-string
  "Test remove-duplicates* on string"
  (let ((result (clysm::remove-duplicates* "abracadabra")))
    ;; Keeps last occurrences, so r, a, b remain
    (ok (stringp result)
        "should return a string")))

(deftest remove-duplicates-with-key
  "Test remove-duplicates* with :key"
  (let ((result (clysm::remove-duplicates* '((a 1) (b 2) (a 3)) :key #'car)))
    (ok (equal result '((b 2) (a 3)))
        "should remove duplicates by key")))

(deftest remove-duplicates-with-test
  "Test remove-duplicates* with :test"
  (let ((result (clysm::remove-duplicates* '("A" "a" "B" "b") :test #'string-equal)))
    (ok (= (length result) 2)
        "should have 2 elements (case-insensitive)")))

(deftest remove-duplicates-no-duplicates
  "Test remove-duplicates* when no duplicates"
  (let ((result (clysm::remove-duplicates* '(a b c d))))
    (ok (equal result '(a b c d))
        "should return copy of input")))
